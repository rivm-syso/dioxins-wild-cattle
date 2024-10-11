source('Models/dioxinCattle/rodeGeusCowWeb.R')
library(ggplot2)
library(magrittr)


#' This script serves to determine dioxin, dl-PCB and TEQ concentration in milk
#' based on the Rode Geus Cow model developed at RIVM.

#' 
#' Date of creation: 24-02-2022
#' Author: J Minnema

#---------- Prepare data ---------- #
intakeGrass <- as.data.frame(read.csv("verification/data/beuningen_grass_intake_standard.csv"))
intakeSoil <- as.data.frame(read.csv("verification/data/beuningen_soil_intake.csv"))

# Define grass and soil concentrations based on measurements
measuredMonthsGrass <- as.integer(format(as.POSIXct(intakeGrass$date),"%m"))
cGrassMin <- mean(c(intakeGrass[measuredMonthsGrass>3 & measuredMonthsGrass<10,]$dioxin_low, 
                    intakeGrass[measuredMonthsGrass>3 & measuredMonthsGrass<10,]$dioxin_high))
cGrassMax <- mean(c(intakeGrass[measuredMonthsGrass<3 | measuredMonthsGrass>10,]$dioxin_low, 
                    intakeGrass[measuredMonthsGrass<3 | measuredMonthsGrass>10,]$dioxin_high))
cSoil <- mean(intakeSoil$dioxin)

  
# -------------- Define scenario's ------------- #
# Standard scenario

solutionWorstCase <- list()

for (i in 1:N) {
  tBirth <- uniqueBirthDay[i]
  parameters <- assign_parameters()
  parameters <- param_update(parameters, TSTOP=365*15, birthDay=tBirth, iGrassAdult=NULL, 
                             cGrassMax=cGrassMax, cGrassMin=cGrassMin,cGrassRef=0.492,
                             iSoilFraction=NULL,cSoil=cSoil,cSoilRef=1.54,
                             TDOSEOFF=NULL)
  solutionWorstCase[[i]] <- run_model(parameters)
  
 }

# Realistic scenario
solutionRealistic <- list()
for (i in 1:N) {
  tBirth <- uniqueBirthDay[i]
  doseoff <- dataBeuningen$doseoff[dataBeuningen$ID==i] 
  parameters <- assign_parameters()
  parameters <- param_update(parameters, TSTOP=365*7, birthDay=tBirth, iGrassAdult=NULL, 
                             cGrassMax=0.70, cGrassMin=NULL,cGrassRef=0.492,
                             iSoilFraction=NULL,cSoil=14,cSoilRef=1.54,
                             TDOSEOFF=doseoff)
  solutionRealistic[[i]] <- run_model(parameters)
  
}


#----------------- Visualization -------------------- #

concWorstCaseMeat <- c()
concWorstCaseLiver <- c()
concWorstCaseBlood <- c()
intakeWorstCase <- c()

bw <- c()
for (jj in 1:N) {
  concWorstCaseMeat <- c(concWorstCaseMeat, solutionWorstCase[[jj]][,"cMeatFat.aSlow"])
  concWorstCaseLiver <- c(concWorstCaseLiver, solutionWorstCase[[jj]][,"cLiver.aLiver"])
  concWorstCaseBlood <- c(concWorstCaseBlood, solutionWorstCase[[jj]][,"cBlood.aBlood"])
  intakeWorstCase <- c(intakeWorstCase, solutionWorstCase[[jj]][,"intake"])
  bw <- c(bw, solutionWorstCase[[jj]][,"bw"])
}

# Create dataframe with relevant information on the simulation
simWorstCase <- data.frame(ID=rep((1:N),each=length(solutionWorstCase[[1]][,"time"])),
                           time=rep(solutionWorstCase[[1]][,"time"],N),
                           concMeat=c(concWorstCaseMeat),
                           concLiver=c(concWorstCaseLiver),
                           concBlood=c(concWorstCaseBlood),
                           bw = bw,
                           intake = c(intakeWorstCase))
                             

concRealisticMeat <- c()
concRealisticLiver <- c()
concRealisticBlood <- c()
intakeRealistic <- c() 

for (jj in 1:N) {
  concRealisticMeat <- c(concRealisticMeat, solutionRealistic[[jj]][,"cMeatFat.aSlow"])
  concRealisticLiver <- c(concRealisticLiver, solutionRealistic[[jj]][,"cLiver.aLiver"])
  concRealisticBlood <- c(concRealisticBlood, solutionRealistic[[jj]][,"cBlood.aBlood"])
  intakeRealistic <- c(intakeRealistic, solutionRealistic[[jj]][,"intake"]) 
}

# Create dataframe with relevant information on the simulation
simRealistic <- data.frame(ID=rep((1:N),each=length(solutionRealistic[[1]][,"time"])),
                           time=rep(solutionRealistic[[1]][,"time"],N),
                           concMeat=c(concRealisticMeat),
                           concLiver=c(concRealisticLiver),
                           concBlood=c(concRealisticBlood))




pltMeat <- ggplot() +
  xlab("Time (years)") +
  ylab("Total TEQ concentration (pg/g meat fat)") +
  geom_line(data=simWorstCase,
            aes(x=time/365, y=concMeat, color=factor(ID)), size=0.5) +
  geom_point(data = dataBeuningen[dataBeuningen$comp=="meatfat",],
             aes(x=observedTimes/365, y=conc, color=factor(ID)),size=1) +
  geom_line(data=simRealistic,
            aes(x=time/365, y=concMeat, color=factor(ID)), linetype=2) +
  scale_color_discrete("Calf ID") + 
  scale_color_manual(values = c("1" = "red",  
                                "2" = "green", 
                                "3" = "black", 
                                "4" = "purple", 
                                "5" = "blue")) +
  
  ggtitle("Dioxin concentration in meat fat") 
  

pltLiver <- ggplot() +
  xlab("Time (years)") +
  ylab("Total TEQ concentration (pg/g liver fat)") +
  geom_line(data=simWorstCase,
            aes(x=time/365, y=concLiver, color=factor(ID))) +
  geom_point(data = dataBeuningen[dataBeuningen$comp=="liver",],
             aes(x=observedTimes/365, y=conc, color=factor(ID))) +
  geom_line(data=simRealistic,
            aes(x=time/365, y=concLiver, color=factor(ID)), linetype=2) +
  scale_color_discrete("Calf ID") + 
  scale_color_manual(values = c("1" = "red",  
                                "2" = "green", 
                                "3" = "black", 
                                "4" = "purple", 
                                "5" = "blue")) +
  ggtitle("Dioxin concentration in liver fat") 

pltBlood <- ggplot() +
  xlab("Time (years)") +
  ylab("Total TEQ concentration (pg/g blood)") +
  geom_line(data=simWorstCase,
            aes(x=time/365, y=concBlood, color=factor(ID))) +
  geom_point(data = dataBeuningen[dataBeuningen$comp=="blood",],
             aes(x=observedTimes/365, y=conc, color=factor(ID))) +
  geom_line(data=simRealistic,
            aes(x=time/365, y=concBlood, color=factor(ID)), linetype=2) +
  scale_color_discrete("Calf ID") + 
  scale_color_manual(values = c("1" = "red",  
                                "2" = "green", 
                                "3" = "black", 
                                "4" = "purple", 
                                "5" = "blue")) +
  ggtitle("Dioxin concentration in blood fat") 


pltBw <- ggplot() +
  xlab("Time (years)") +
  ylab("Total body weight (kg)") +
  geom_line(data=simWorstCase,
            aes(x=time/365, y=bw, color=factor(ID))) +
  scale_color_discrete("Calf ID") + 
  scale_color_manual(values = c("1" = "red",  
                                "2" = "green", 
                                "3" = "black", 
                                "4" = "purple", 
                                "5" = "blue")) +
  ggtitle("Body weight of the Rode Geus cows") 

pltIntakeWorstCase <- ggplot() +
  xlab("Time (years)") +
  ylab("Intake") +
  geom_line(data=simWorstCase,
            aes(x=time/365, y=intake, color=factor(ID))) +
  scale_color_discrete("Calf ID") + 
  scale_color_manual(values = c("1" = "red",  
                                "2" = "green", 
                                "3" = "black", 
                                "4" = "purple", 
                                "5" = "blue")) +
  ggtitle("Intake") 










  




