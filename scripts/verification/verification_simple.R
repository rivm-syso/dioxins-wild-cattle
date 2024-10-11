#' This script is intended to verify the calibrated RodeGeus Model.
#' 
#' 
#' Verification of the model is only kidney fat. Although this is the main
#' marker of the model, other concentrations e.g., blood fat concentrations, 
#' meat fat concentrations and liver concentrations have not been validated.
#' 

library(ggplot2)
library(gridExtra)

# Load model
source("model/RodeGeusWeb.R")

# load verification data
source("scripts/data_processing/process_verification_data.R")

# Load congener-specific kinetic data
kin_data <- read.csv("scripts/calibration/results.csv", header=TRUE)
tef <- read.csv("scripts/verification/tef_values.csv")
tef_id <- "2005" # Either select 2005, or 2022

# set up time unit
unit <- 'days'
tUnit <- 24*60*60 # number of seconds in a unit of time

# Set up for loops
congeners <- unique(verificationData$congener)
ids <- c("15","22")
solution <- list()


gender <- c("cow","bull")

# Loop over floodplains
for (f in 1:2) {
  
  data <- verificationData[verificationData$ID == ids[f],]
  
  # Loop over cattle (1 cow, 1 bull)
  for (id in 1:2) {
    if (f == 2 & id == 2) next
    
    parameters <- assign_parameters()
    
    # update individual characteristics
    parameters <- utilities::param_update(parameters,
                                          unit = unit,
                                          gender = gender[id],
                                          TSTOP = 1400, 
                                          birthDay = "2020-04-01",
                                          simFlood = FALSE,
                                          simClean = FALSE)
    
    for (c in 1:length(congeners)) {
      
      i <- c + length(congeners) * (id + 2*(f-1) - 1)
      
      # select congener specific data
      data_congener <- data[data$congener == congeners[c],]
      data_kin_congener <- kin_data[kin_data$congener == congeners[c],]
      
      # Update congener specific exposure characteristics
      parameters <- utilities::param_update(parameters, 
                                            cGrassMax=unique(data_congener$cGrassMax), 
                                            cGrassMin=unique(data_congener$cGrassMin),
                                            cMilk = 0, 
                                            cSoil=unique(data_congener$cSoil))
      
      # Update congener specific kinetic characteristics
      parameters <- utilities::param_update(parameters, 
                                            pFat=data_kin_congener$pFat_median,
                                            pLiver =data_kin_congener$pLiver_median ,
                                            pSlow=data_kin_congener$pSlow_median,
                                            kMet=data_kin_congener$kMet_median,
                                            fAbs=data_kin_congener$fAbs_median)
      
      # Run simulation
      startYear <- as.POSIXct(paste0(strsplit("2020-04-01",split="-")[[1]][1], "-01-01"))
      solution[[i]] <- as.data.frame(run_model(parameters))
      endDate <- startYear + as.difftime(length(solution[[i]]$time) - 1, units=unit)
      solution[[i]]$date <- seq(from=startYear, 
                                by=tUnit, 
                                to=endDate)
      }
  }
}

#----------------- Structure data -------------------- #

ID <- c()
times <- as.Date(c())
concMeatFat <- c()
Compound <- c()
Gender <- c()
tef_ <- c()
Floodplains <- c()

for (ff in 1:2) {
  for (idd in 1:length(ids)) {
    if (ff == 2 & idd == 2) next
    for (cc in 1:length(congeners)) {
      ii <- cc + length(congeners) * (idd + 2*(ff-1) - 1)
      
      # Select tef values
      if (tef_id == "2005") {
        tef_val <- tef[tef$Compound == congeners[cc],]$tef2005  
        }
      else if (tef_id == "2022") {
        tef_val <- tef[tef$Compound == congeners[cc],]$tef2022
      }
      
      nr <- nrow(solution[[ii]])
      times <- c(times, solution[[ii]][,"date"])
      ID <- c(ID, rep(ids[idd], nr))
      Floodplains <- c(Floodplains, rep(ff, nr))
      Compound <- c(Compound, rep(congeners[cc], nr))
      Gender <- c(Gender, rep(gender[idd], nr))
      concMeatFat <- c(concMeatFat, solution[[ii]][,"cMeatFat.aSlow"])
      tef_ <- c(tef_, rep(tef_val, nr))
      
    }
  }
}


# Create dataframe with relevant information on the simulation
simulation <- data.frame(ID=ID,
                         Gender=c(Gender),
                         time=c(times),
                         Floodplains=c(Floodplains),
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
  dplyr::group_by(ID, Floodplains, time) %>% 
  dplyr::summarise(concMeatFat = sum(TEQmeat), Compound =  "Sum TEQ", Gender=Gender) %>% 
  rbind(simulation)

fp <- c("Beuningen", "Loevestein")
for (fff in 1:2) {
  for (ccc in 1:length(congeners)) {
    
    sim <- simulation[simulation$Compound == congeners[ccc],]
    teq <- teq + sim[c("concMeatFat")] * sim$tef_
    
  }

  # Separate by floodplains
  sim <- theSimulation[theSimulation$Compound == 'Sum TEQ' & theSimulation$Floodplains==fff,]
  obs <- verificationData[verificationData$congener == "Total-TEQ",]
  ifelse(fff == 1,
         obs <- obs[obs$ID %in% c("15","16","17","18","19","20","21"),],
         obs <- obs[obs$ID %in% c("22","23","24","25","26","27","28"),])
  
  # Plot
  pltMeatFat[[fff]] <- ggplot() +
    xlab("Time (years)") +
    ylab("Concentration (pg TEQ/g fat)") +
    geom_point(data = obs,
               aes(x=as.Date(Date), y=obs_cMeatFat.aSlow,color=factor(gender)),size=4) +
    geom_line(data=sim,
              aes(x=time, y=concMeatFat, color=factor(Gender)), linetype=2) +
    scale_color_manual(values=c("#9999CC")) +
    scale_x_date(date_labels = "%Y") +
    ggtitle(fp[fff])
}

print(pltMeatFat)
