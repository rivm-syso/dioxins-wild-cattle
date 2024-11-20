#' This script is intended to verify the calibrated Rode Geus Model.
#' 
#' 
#' Verification of the model is only kidney fat. Although this is the main
#' marker of the model, other concentrations e.g., blood fat concentrations, 
#' meat fat concentrations and liver concentrations have not been validated.
#' 

library(ggplot2)
library(gridExtra)

# Load model
source("model/model.R")

# load verification data
source("scripts/data_processing/process_validation_data.R")

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
solution <- list()

# Loop over cattle
for (id in 1:length(ids)) {
  data <- verificationData[verificationData$ID == ids[id],]
  
  
  parameters <- assign_parameters()
  
  # update individual characteristics
  parameters <- utilitiesVVH::param_update(parameters, 
                                        unit = unit,
                                        gender = unique(data$gender),
                                        TSTOP=unique(data$TSTOP)+10, 
                                        birthDay=unique(data$birthDay),
                                        simFlood=unique(data$simFlood),
                                        tFlood = unique(data$tFlood),
                                        dFlood = unique(data$dFlood),
                                        tClean = unique(data$tClean)+10)
  
  for (c in 1:length(congeners)) {
   
     # Do check for kinetic parameters
    if (!(congeners[c] %in% kin_data$congener)){
      next
    } 
    
    # select congener specific data
    data_congener <- data[data$congener == congeners[c],]
    data_kin_congener <- kin_data[kin_data$congener == congeners[c],]
   
    # Update congener specific exposure characteristics
    parameters <- utilitiesVVH::param_update(parameters, 
                                          cGrassMax=unique(data_congener$cGrassMax), 
                                          cGrassMin=unique(data_congener$cGrassMin),
                                          cMilk = 0, 
                                          cSoil=unique(data_congener$cSoil))
    
    # Update congener specific kinetic characteristics
    parameters <- utilitiesVVH::param_update(parameters, 
                                          pFat=data_kin_congener$pFat_median,
                                          pLiver =data_kin_congener$pLiver_median ,
                                          pSlow=data_kin_congener$pSlow_median,
                                          kMet=data_kin_congener$kMet_median,
                                          fAbs=data_kin_congener$fAbs_median)
    
    
    
    # Run simulation
    startYear <- as.POSIXct(paste0(strsplit(unique(data_congener$birthDay),split="-")[[1]][1], "-01-01"))
    solution[[(id-1)*length(congeners) + c]] <- as.data.frame(run_model(parameters))
    endDate <- startYear + as.difftime(length(solution[[(id-1)*length(congeners) + c]]$time) - 1, units=unit)
    solution[[(id-1)*length(congeners) + c]]$date <- seq(from=startYear, 
                                                         by=tUnit, 
                                                         to=endDate)
    
    
  }
}

#----------------- Structure data -------------------- #

ID <- c()
times <- as.Date(c())
concMeatFat <- c()
Compound <- c()
Gender <- c()
tef_ <- c()

for (idd in 1:length(ids)) {
  for (cc in 1:length(congeners)) {
    
    # Do check for kinetic parameters
    if (!(congeners[cc] %in% kin_data$congener)){
      next
    } 
    
    # Select tef values
    if (tef_id == "2005") {
      if (congeners[cc]=="TEQ2005") tef_val <- tef[tef$Compound == "Total-TEQ",]$tef2005  
      else tef_val <- tef[tef$Compound == congeners[cc],]$tef2005  
    }
    else if (tef_id == "2022") {
      if (congeners[cc]=="TEQ2022") tef_val <- tef[tef$Compound == "Total-TEQ",]$tef2005  
      else tef_val <- tef[tef$Compound == congeners[cc],]$tef2005  
      tef_val <- tef[tef$Compound == congeners[cc],]$tef2022
    }
    
    nr <- nrow(solution[[(idd-1)*length(congeners) + cc]])
    times <- c(times, solution[[(idd-1)*length(congeners) + cc]][,"date"])
    ID <- c(ID, rep(ids[idd], nr))
    Compound <- c(Compound, rep(congeners[cc], nr))
    Gender <- c(Gender, rep(unique(verificationData[verificationData$ID==idd,]$gender), nr))
    concMeatFat <- c(concMeatFat, solution[[(idd-1)*length(congeners) + cc]][,"cMeatFat.aSlow"])
    tef_ <- c(tef_, rep(tef_val, nr))
  }
}

# Create dataframe with relevant information on the simulation
simulation <- data.frame(ID=ID,
                         time=c(times),
                         Compound=c(Compound),
                         concMeatFat=c(concMeatFat),
                         tef_=tef_)
simulation$time <- as.Date(simulation$time)

# ----------------------------------------------------#
#----------------- Visualization -------------------- #
# ----------------------------------------------------#

# Plot per congener
pltMeatFat <- list()
teq <- 0 # Counter for calculating the total TEQ value
i <- 0


theSimulation <- simulation %>% 
  mutate(TEQmeat = concMeatFat * tef_) %>%
  dplyr::group_by(ID, time) %>% 
  dplyr::summarise(concMeatFat = sum(TEQmeat), Compound =  "Sum TEQ") %>% 
  rbind(simulation)

# Remove times before birth
for (id in ids) {
  theSimulation <- theSimulation %>% filter(ID != id | ID==id & time>as.Date(unique(verificationData[verificationData$ID==id,"birthDay"])[[1]]))
}

# Remove 365 days after birth
for (id in ids) {
  oneYearAfterBirth <- unique(as.Date(unique(verificationData[verificationData$ID==id,"birthDay"])[[1]]))+365
  theSimulation <- theSimulation %>% filter(ID != id | ID==id & time>oneYearAfterBirth)
}


for (ccc in 1:length(congeners)) {
  # Do check for kinetic parameters
  if (!(congeners[ccc] %in% kin_data$congener)){
    next
  } 
  i <- i + 1

  sim <- theSimulation[theSimulation$Compound == congeners[ccc],] %>% filter(time > "2019-01-01")
  teq <- teq + sim[c("concMeatFat")] * sim$tef_
  obs <- verificationData[verificationData$congener == congeners[ccc],]
  pltMeatFat[[i]] <- ggplot() +
    xlab("Time (years)") +
    ylab("Concentration (pg/g fat)") +
    geom_point(data = obs,
               aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID)),size=4) +
    geom_line(data=sim,
              aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
    scale_color_discrete("Animal ID") + 
    scale_x_date(date_labels = "%Y") +
    ggtitle(paste(congeners[ccc], " in muscle fat"))
}

sim <- theSimulation[theSimulation$Compound == 'Sum TEQ',]
obs <- verificationData[verificationData$congener == 'TEQ2005',]

pltMeatFat[[i+1]] <- ggplot() +
  xlab("Time (years)") +
  ylab("Total TEQ concentration (pg TEQ/g fat)") +
  geom_point(data = obs,
             aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID)),size=4) +
  geom_line(data=sim,
            aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
  #scale_y_continuous(trans='log10') +
  scale_x_date(date_labels = "%Y") +
  ggtitle("Sum of individual congeners") 


# Save plots
plt <- 0
for (ii in 1:length(congeners)) {
  # Do check for kinetic parameters
  if (!(congeners[ii] %in% kin_data$congener)) {
    next
  } 
  
  plt <- plt + 1
  ggsave(filename=paste0("scripts/validation/figures/meatfat/meatfat_",congeners[ii] ,".png"),
         plot=pltMeatFat[[plt]],
         width=3000,
         height=2000,
         units="px",
         dpi=300) 
  
}

ggsave(filename=paste0("scripts/validation/figures/meatfat/meatfat_sumTEQ.png"),
       plot=pltMeatFat[[plt+1]],
       width=3000,
       height=2000,
       units="px",
       dpi=300) 


#@@@@@@@@@@@@@@@@@@@@
# Create Figures


sim_sumTEQ_beuningen <- theSimulation[theSimulation$Compound == 'Sum TEQ',] %>% filter(ID %in% c(15,16,17,18,19,20,21))
sim_beuningen <- theSimulation %>% filter(ID %in% c(15,16,17,18,19,20,21))
obs_beuningen <- verificationData %>% filter(ID %in% c(15,16,17,18,19,20,21))

sim_sumTEQ_loevestein <- theSimulation[theSimulation$Compound == 'Sum TEQ',] %>% filter(ID %in% c(22,23,24,25,26,27,28))
sim_loevestein <- theSimulation %>% filter(ID %in% c(22,23,24,25,26,27,28))
obs_loevestein <- verificationData %>% filter(ID %in% c(22,23,24,25,26,27,28))

# Figure 5
sim_totalTEQ_beuningen <- sim_beuningen[sim_beuningen$Compound == "TEQ2005",] %>% filter(time > "2019-01-01")
sim_sumTEQ_beuningen <- sim_sumTEQ_beuningen %>% filter(time > "2019-01-01")
obs_totalTEQ_beuningen <- obs_beuningen[obs_beuningen$congener == "TEQ2005",]

pltsumTEQ_beuningen <- ggplot() +
  xlab("Date (years)") +
  ylab("Concentration (pg/g fat)") +
  geom_point(data = obs_totalTEQ_beuningen,
             aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID)),size=2) +
  geom_line(data=sim_sumTEQ_beuningen,
            aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
  scale_x_date(date_labels = "%Y") +
  ylim(0,80) + 
  ggtitle("TEQ in muscle fat: congener-specific approach")

plttotalTEQ_beuningen <- ggplot() +
  xlab("Date (years)") +
  ylab("Concentration (pg/g fat)") +
  geom_point(data = obs_totalTEQ_beuningen,
             aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID)),size=2) +
  geom_line(data=sim_totalTEQ_beuningen,
            aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
  scale_x_date(date_labels = "%Y") +
  ylim(0,80) + 
  ggtitle("TEQ in muscle fat: total-TEQ approach")

# Figure 6
sim_totalTEQ_loevestein <- sim_loevestein[sim_loevestein$Compound == "TEQ2005",] %>% filter(time > "2019-01-01")
sim_sumTEQ_loevestein <- sim_sumTEQ_loevestein %>% filter(time > "2019-01-01")
obs_totalTEQ_loevestein <- obs_loevestein[obs_loevestein$congener == "TEQ2005",]

pltsumTEQ_loevestein <- ggplot() +
  xlab("Date (years)") +
  ylab("Concentration (pg/g fat)") +
  geom_point(data = obs_totalTEQ_loevestein,
             aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID)),size=2) +
  geom_line(data=sim_sumTEQ_loevestein,
            aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
  scale_x_date(date_labels = "%Y") +
  ylim(0,60) + 
  ggtitle("TEQ in muscle fat: congener-specific approach")

plttotalTEQ_loevestein <- ggplot() +
  xlab("Date (years)") +
  ylab("Concentration (pg/g fat)") +
  geom_point(data = obs_totalTEQ_loevestein,
             aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID)),size=2) +
  geom_line(data=sim_totalTEQ_loevestein,
            aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
  scale_x_date(date_labels = "%Y") +
  ylim(0,60) + 
  ggtitle("TEQ in muscle fat: total-TEQ approach")

gridBeuningen <- gridExtra::grid.arrange(pltsumTEQ_beuningen,plttotalTEQ_beuningen, ncol=1)
gridLoevestein <- gridExtra::grid.arrange(pltsumTEQ_loevestein,plttotalTEQ_loevestein, ncol=1)

# Save plots
ggsave(filename=paste0("figures/Fig5.jpg"),
         plot=gridBeuningen,
         width=3000,
         height=2000,
         units="px",
         dpi=300)
ggsave(filename=paste0("figures/Fig6.jpg"),
       plot=gridLoevestein,
       width=3000,
       height=2000,
       units="px",
       dpi=300) 


