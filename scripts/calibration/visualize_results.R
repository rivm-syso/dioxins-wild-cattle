library(ggplot2)
library(dplyr)

# Load model
source("model/RodeGeusWeb.R")

# Get results 
source("scripts/calibration/process_results.R")

# Read calibration data
source("scripts/data_processing/process_calibration_data_v2.R")

# Read calibration results
kin_data <- read.csv("scripts/calibration/results.csv", header=TRUE)

# TEF values to use
tef <- read.csv("scripts/verification/tef_values.csv")
tef_id <- "2005" # Either select 2005, or 2022


# Cattle IDs
congeners <- unique(calibrationData$congener)
ids <- unique(calibrationData$ID)
solution <- list()

# Loop over cattle
for (id in 1:length(ids)) {
  data <- calibrationData[calibrationData$ID == ids[id],]
  
  # update individual characteristics
  parameters <- assign_parameters()
  parameters <- utilities::param_update(parameters, 
                                        unit = 'days',
                                        gender = unique(data$gender),
                                        TSTOP=unique(data$TSTOP), 
                                        birthDay=unique(data$birthDay),
                                        simFlood=unique(data$simFlood),
                                        tFlood = unique(data$tFlood),
                                        dFlood = unique(data$dFlood),
                                        tClean = unique(data$tClean),
                                        simClean = TRUE)
  
  for (c in 1:length(congeners)) {
    
    # Do check for kinetic parameters
    if (!(congeners[c] %in% kin_data$congener)){
      next
    } 
    
    # select congener specific data
    data_congener <- data[data$congener == congeners[c],]
    data_kin_congener <- kin_data[kin_data$congener == congeners[c],]
    
    # Update congener specific exposure characteristics
    parameters <- utilities::param_update(parameters, 
                                          cGrassMax=unique(data_congener$cGrassMax), 
                                          cGrassMin=unique(data_congener$cGrassMin),
                                          cGrassClean=unique(data_congener$cGrassClean),
                                          cMilk = 0, # TODO
                                          cSoil=unique(data_congener$cSoil))
    
    # Update congener specific kinetic characteristics
    parameters <- utilities::param_update(parameters, 
                                          pFat=data_kin_congener$pFat_median,
                                          pLiver =data_kin_congener$pLiver_median ,
                                          pSlow=data_kin_congener$pSlow_median,
                                          kMet=data_kin_congener$kMet_median,
                                          fAbs=data_kin_congener$fAbs_median)
    
    
    
    # Run simulation
    startYear <- as.POSIXct(paste0(strsplit(unique(data_congener$birthDay),split="-")[[1]][1], "-01-01"))
    solution[[(id-1)*length(congeners) + c]] <- as.data.frame(run_model(parameters))
    endDate <- startYear + as.difftime(length(solution[[(id-1)*length(congeners) + c]]$time) - 1, units="days")
    solution[[(id-1)*length(congeners) + c]]$date <- seq(from=startYear, 
                                                         by=24*60*60, 
                                                         to=endDate)
    
    
  }
}


ID <- c()
times <- as.Date(c())
concMeatFat <- c()
concLiver <- c()
concBlood <- c()
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
      tef_val <- tef[tef$Compound == congeners[cc],]$tef2005  
    }
    else if (tef_id == "2022") {
      tef_val <- tef[tef$Compound == congeners[cc],]$tef2022
    }
    
    nr <- nrow(solution[[(idd-1)*length(congeners) + cc]])
    times <- c(times, solution[[(idd-1)*length(congeners) + cc]][,"date"])
    ID <- c(ID, rep(ids[idd], nr))
    Compound <- c(Compound, rep(congeners[cc], nr))
    Gender <- c(Gender, rep(unique(calibrationData[calibrationData$ID==idd,]$gender), nr))
    concMeatFat <- c(concMeatFat, solution[[(idd-1)*length(congeners) + cc]][,"cMeatFat.aSlow"])
    concLiver <- c(concLiver, solution[[(idd-1)*length(congeners) + cc]][,"cLiver.aLiver"])
    concBlood <- c(concBlood, solution[[(idd-1)*length(congeners) + cc]][,"cBloodFat.aBlood"])
    tef_ <- c(tef_, rep(tef_val, nr))
  }
}

# Create dataframe with relevant information on the simulation
simulation <- data.frame(ID=ID,
                         time=c(times),
                         Compound=c(Compound),
                         concMeatFat=c(concMeatFat),
                         concLiver=c(concLiver),
                         concBlood=c(concBlood),
                         tef_=tef_)
simulation$time <- as.Date(simulation$time)


# ----------------------------------------------------#
#----------------- Visualization -------------------- #
# ----------------------------------------------------#

# Plot per congener
pltMeatFat <- list()
pltLiver <- list()
pltBlood <- list()

teqMeatFat <- 0 # Counter for calculating the total TEQ value
teqLiver <- 0 # Counter for calculating the total TEQ value
teqBlood <- 0 # Counter for calculating the total TEQ value
i <- 0


theSimulation <- simulation %>% 
  mutate(TEQmeat = concMeatFat * tef_, TEQliver = concLiver * tef_, TEQblood = concBlood * tef_) %>%
  group_by(ID, time) %>% 
  dplyr::summarize(concMeatFat = sum(TEQmeat), concLiver = sum(TEQliver), concBlood = sum(TEQblood), Compound =  "Sum TEQ") %>% 
  rbind(simulation)


for (ccc in 1:length(congeners)) {
  # Do check for kinetic parameters
  if (!(congeners[ccc] %in% kin_data$congener)){
    next
  } 
  i <- i + 1
  
  sim <- simulation[simulation$Compound == congeners[ccc],]
  obs <- calibrationData[calibrationData$congener == congeners[ccc],]

  pltMeatFat[[i]] <- ggplot() +
    xlab("Time (years)") +
    ylab("Concentration (pg/g kidney fat)") +
    geom_point(data = obs,
               aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID),shape=factor(gender)),size=2) +
    geom_line(data=sim,
              aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
    scale_color_discrete("Animal ID") + 
    scale_x_date(date_labels = "%Y") +
    ggtitle(congeners[ccc])
  
  pltLiver[[i]] <- ggplot() +
    xlab("Time (years)") +
    ylab("Concentration (pg/g liver)") +
    geom_point(data = obs,
               aes(x=as.Date(Date), y=obs_cLiver.aLiver, color=factor(ID),shape=factor(gender)),size=2) +
    geom_line(data=sim,
              aes(x=time, y=concLiver, color=factor(ID)), linetype=2) +
    scale_color_discrete("Animal ID") + 
    scale_x_date(date_labels = "%Y") +
    ggtitle(congeners[ccc])
  
  pltBlood[[i]] <- ggplot() +
    xlab("Time (years)") +
    ylab("Concentration (pg/g plasma fat)") +
    geom_point(data = obs,
               aes(x=as.Date(Date), y=obs_cBloodFat.aBlood, color=factor(ID),shape=factor(gender)),size=2) +
    geom_line(data=sim,
              aes(x=time, y=concBlood, color=factor(ID)), linetype=2) +
    scale_color_discrete("Animal ID") + 
    scale_x_date(date_labels = "%Y") +
    ggtitle(congeners[ccc])

}

sim <- theSimulation[theSimulation$Compound == 'Sum TEQ',]
obs <- calibrationData[calibrationData$congener == 'Total-TEQ',]

pltMeatFat[[i+1]] <- ggplot() +
  xlab("Time (years)") +
  ylab("Total TEQ concentration (pg TEQ/g kidney fat)") +
  geom_point(data = obs,
             aes(x=as.Date(Date), y=obs_cMeatFat.aSlow, color=factor(ID),shape=factor(gender)),size=2) +
  geom_line(data=sim,
            aes(x=time, y=concMeatFat, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
  scale_x_date(date_labels = "%Y") +
  ggtitle("Sum of individual congeners")

pltLiver[[i+1]] <- ggplot() +
  xlab("Time (years)") +
  ylab("Concentration (pg/g liver)") +
  geom_point(data = obs,
             aes(x=as.Date(Date), y=obs_cLiver.aLiver, color=factor(ID),shape=factor(gender)),size=2) +
  geom_line(data=sim,
            aes(x=time, y=concLiver, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
  scale_x_date(date_labels = "%Y") +
  ggtitle("Sum of individual congeners")

pltBlood[[i+1]] <- ggplot() +
  xlab("Time (years)") +
  ylab("Concentration (pg/g plasma fat)") +
  geom_point(data = obs,
             aes(x=as.Date(Date), y=obs_cBloodFat.aBlood, color=factor(ID),shape=factor(gender)),size=2) +
  geom_line(data=sim,
            aes(x=time, y=concBlood, color=factor(ID)), linetype=2) +
  scale_color_discrete("Animal ID") + 
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
  ggsave(filename=paste0("scripts/calibration/figures/meatfat/meatfat_",congeners[ii] ,".png"),
         plot=pltMeatFat[[plt]],
         width=1600,
         height=1600,
         units="px",
         dpi=300)
  ggsave(filename=paste0("scripts/calibration/figures/bloodfat/bloodfat_",congeners[ii] ,".png"),
         plot=pltBlood[[plt]],
         width=1600,
         height=1600,
         units="px",
         dpi=300)
  ggsave(filename=paste0("scripts/calibration/figures/liverfat/liverfat_",congeners[ii] ,".png"),
         plot=pltLiver[[plt]],
         width=1600,
         height=1600,
         units="px",
         dpi=300)
}


ggsave(filename=paste0("scripts/calibration/figures/meatfat/meatfat_sumTEQ.png"),
       plot=pltMeatFat[[plt+1]],
       width=1600,
       height=1600,
       units="px",
       dpi=300)
ggsave(filename=paste0("scripts/calibration/figures/bloodfat/bloodfat_sumTEQ.png"),
       plot=pltBlood[[plt+1]],
       width=1600,
       height=1600,
       units="px",
       dpi=300)
ggsave(filename=paste0("scripts/calibration/figures/liverfat/liverfat_sumTEQ.png"),
       plot=pltLiver[[plt+1]],
       width=1600,
       height=1600,
       units="px",
       dpi=300)









