#' This script is intended to analyze the confidence intervals of the predicted muscle fat concentrations


library(ggplot2)
library(gridExtra)

# Load model
source("model/model.R")

# load verification data
source("scripts/data_processing/process_validation_data.R")

# Load helper function
source("functions/helper_functions.R")

# Load congener-specific kinetic data
kin_data <- read.csv("scripts/calibration/results.csv", header=TRUE)
tef <- read.csv("scripts/validation/tef_values.csv")
tef_id <- "2005" # Either select 2005, or 2022. TEFs have been revised in 2022

# Set up time unit
unit <- 'days'
tUnit <- 24*60*60 # number of seconds in a day

# Set up for loops
congeners <- unique(verificationData$congener)
congeners <- congeners[1:30]

ids <- unique(as.integer(verificationData$ID))
solution <- c()
solution_teq <- 0


set.seed(1234)

n_sample <- 20



# Select TEQ2005 data
data <- verificationData[verificationData$ID == 15 & verificationData$congener == "TEQ2005",]
startYear <- as.POSIXct(paste0(strsplit(unique(data$birthDay),split="-")[[1]][1], "-01-01"))




# Loop over congeners
for (cong in 1:length(congeners)) {
  
  print(paste("** Congener", cong))
  
  # Get kinetic parameters
  kin_data_full <- read.delim(paste0("scripts/calibration/fit/", congeners[cong], "/run1/chains/equal_weighted_post.txt"), sep=" ", header = TRUE)
  
  # Sample parameters
  sample_pfat <- sample(1:nrow(kin_data_full), n_sample)
  sample_pliver <- sample(1:nrow(kin_data_full), n_sample)
  sample_pslow <- sample(1:nrow(kin_data_full), n_sample)
  sample_kmet <- sample(1:nrow(kin_data_full), n_sample)
  sample_fabs <- sample(1:nrow(kin_data_full), n_sample)
  
  pb = txtProgressBar(min = 0, max = n_sample, initial = 0, style=3) 
  
  solution_cong <- c()
  
  # Loop over samples  
  for (i in 1:n_sample) {
  
    parameters <- assign_parameters()
    
    # update individual characteristics
    parameters <- param_update(parameters, 
                               unit = unit,
                               gender = unique(data$gender),
                               TSTOP=unique(data$TSTOP)+10, 
                               birthDay=unique(data$birthDay),
                               simFlood=unique(data$simFlood),
                               tFlood = unique(data$tFlood),
                               dFlood = unique(data$dFlood),
                               tClean = unique(data$tClean)+10)
    
    # Update congener specific exposure characteristics
    parameters <- param_update(parameters, 
                               cGrassMax=unique(data$cGrassMax), 
                               cGrassMin=unique(data$cGrassMin),
                               cMilk = 0, 
                               cSoil=unique(data$cSoil))
    
    # Update congener specific kinetic characteristics
    parameters <- param_update(parameters, 
                               pFat=kin_data_full[sample_pfat[i], "pFat"],
                               pLiver=kin_data_full[sample_pliver[i], "pLiver"],
                               pSlow=kin_data_full[sample_pslow[i], "pSlow"],
                               kMet=kin_data_full[sample_kmet[i], "kMet"],
                               fAbs=kin_data_full[sample_fabs[i], "fAbs"])
    
    # Run simulation
    solution_i <- as.data.frame(run_model(parameters))
    
    # Assign correct date
    endDate <- startYear + as.difftime(length(solution_i$time) - 1, units=unit)
    solution_i$date <- seq(from=startYear, 
                           by=tUnit, 
                           to=endDate)
    solution_i$sample_id <- i
    
    solution_cong <- rbind(solution_cong, solution_i)
    setTxtProgressBar(pb,i)
  }
  
  
  
  if (cong != 30) {
    solution_cong$cMeatFat_teq <- solution_cong$cMeatFat.aSlow * tef[tef$Compound == congeners[cong],'tef2005']
    solution_dates <- solution_cong$date
    solution_sum_teq <- solution_sum_teq + solution_cong$cMeatFat_teq
  }
  else solution_total_teq <- solution_cong$cMeatFat.aSlow
  
  close(pb)
  }


simulation_df <- data.frame(dates = as.Date(solution_dates))
simulation_df$sumTEQ <- solution_sum_teq
simulation_df$totalTEQ <- solution_total_teq



# Get median, 5th and 95th percentiles for each date
sim <- simulation_df %>% group_by(dates) %>%
  summarise(dates=dates,
            MedianSumTEQ=median(sumTEQ),
            P5SumTEQ=sort(sumTEQ)[round(n_sample*0.05)],
            P95SumTEQ=sort(sumTEQ)[round(n_sample*0.95)],
            MedianTotalTEQ=median(totalTEQ),
            P5TotalTEQ=sort(totalTEQ)[round(n_sample*0.05)],
            P95TotalTEQ=sort(totalTEQ)[round(n_sample*0.95)])

# Plot
plt_validation <- sim %>% ggplot() +
  geom_line(aes(dates,MedianSumTEQ)) + 
  geom_ribbon(aes(x=dates,ymin=P5SumTEQ,ymax=P95SumTEQ), fill="black", alpha=0.1) +
  geom_point(aes(x=as.Date("2023-11-07"), y=3.52)) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%b %Y") +
  ggtitle("TEQ in muscle fat: 2,3,7,8-TCDD") + 
  xlab("Date") +
  ylab("Concentration (pg/g fat)") 

# ggsave(filename=paste0("figures/FigS3.jpg"),
#        plot=plt_validation,
#        width=3000,
#        height=2000,
#        units="px",
#        dpi=300)

