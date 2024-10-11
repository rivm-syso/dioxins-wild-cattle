#' This script is used to compute the sensitivity of the parameters in the bull
#' and cow models. 

# Load data
source('model/rodeGeusWeb.R')
kin_data <- read.csv("scripts/calibration/results.csv", header=TRUE)
kin_data <- kin_data[kin_data$congener=='TEQ2005',]


#----------------- Bull model -------------------- #

params_bull <- assign_parameters()
params_bull <- utilitiesVVH::param_update(params_bull, 
                                       gender="bull",
                                       TSTOP=365*6,
                                       pFat=kin_data$pLiver_median,
                                       pLiver=kin_data$pLiver_median,
                                       pSlow=kin_data$pSlow_median,
                                       kMet=kin_data$kMet_median,
                                       fAbs=kin_data$fAbs_median)

include_bull <- names(params_bull)[!names(params_bull) %in% c("gender", "TDOSEOFF", "cSoilRef", "cGrassRef", "birthDay", 
                                 "tWin", "tSum", "CINT", "POINTS", "TSTOP", 
                                 "simClean", "simFlood", "tLact", "milkProd", "fatPerc",
                                 "pMilkFat", "q0Milk",
                                 "cGrassClean","cSoilClean","tClean","iSoilFractionFlood", #exposure scenario
                                 "dFlood", "tFlood","cSoil","cGrassMin", "cGrassMax", "simClean",
                                 "simFlood", "unit", "tStartGrassIntake", "cMilk", "iMilk", "iSoilFractionWinter", 
                                 "iSoilFractionSummer", "kGrass", "relFatVariation", "bwRef", "tYear",
                                 "rVBloodCow", "rVSlowCow", "rVRichCow", "rVFatCow","rVLiverCow")] #exposure scenario

sens_bull <- utilitiesVVH::sensitivity_analysis(model_path='model/rodeGeusWeb.R',
                                             parameters=params_bull,
                                             include_parameters=include_bull,
                                             c("cMeatFat.aSlow", "cLiver.aLiver"))
   

#----------------- Cow model -------------------- #
params_cow <- assign_parameters()
params_cow <- utilitiesVVH::param_update(params_cow, 
                                      gender="cow",
                                      TSTOP=365*6,
                                      pFat=kin_data$pFat_median,
                                      pLiver =kin_data$pLiver_median ,
                                      pSlow=kin_data$pSlow_median,
                                      kMet=kin_data$kMet_median,
                                      fAbs=kin_data$fAbs_median)
include_cow <- names(params_cow)[!names(params_cow) %in% c("gender", "TDOSEOFF", "cSoilRef", "cGrassRef", "birthDay", 
                                                              "tWin", "tSum", "CINT", "POINTS", "TSTOP", 
                                                              "simClean", "simFlood", 
                                                              "cGrassClean","cSoilClean","tClean","iSoilFractionFlood", #exposure scenario
                                                              "dFlood", "tFlood","cSoil","cGrassMin", "cGrassMax", "simClean",
                                                              "simFlood", "unit", "tStartGrassIntake", "cMilk", "iMilk", "iSoilFractionWinter", 
                                                              "iSoilFractionSummer", "kGrass", "relFatVariation", "bwRef", "tYear",
                                                              "rVBloodBull", "rVSlowBull", "rVRichBull", "rVFatBull","rVLiverBull")] #exposure scenario
sens_cow <- utilitiesVVH::sensitivity_analysis(model_path='model/rodeGeusWeb.R',
                                            parameters=params_cow,
                                            include_parameters=include_cow,
                                            c("cMeatFat.aSlow", "cLiver.aLiver"))


#----------------- Plot -------------------- #
# for (comp in c("cMeatFat.aSlow", "cLiver.aLiver")) {
#   
#   par(mar=c(15,  5,5 ,5))
#   
#   if (comp=="cMeatFat.aSlow") comp_name <- "meat fat concentration"
#   if (comp=="cLiver.aLiver") comp_name <- "liver concentration"
#   
#   barplot(sens_bull[comp,], ylim=c(-1,1), main=paste0("Bull - sensitivity analysis of ", comp_name, sep=" "), las=2)
#   barplot(sens_cow[comp,], ylim=c(-1,1), main=paste0("Cow - Sensitivity analysis of ", comp_name, sep=" "), las=2)
# }

library(ggplot2)
library(gridExtra)


sens_cow <- as.data.frame(sens_cow)
sens_bull <- as.data.frame(sens_bull)
sens_cow$tissue <- rownames(sens_cow)
sens_bull$tissue <- rownames(sens_bull)

sens_cow_ <- tidyr::pivot_longer(as.data.frame(sens_cow), !tissue, names_to="parameter", values_to="elasticity_coefficient")
sens_bull_ <- tidyr::pivot_longer(as.data.frame(sens_bull), !tissue, names_to="parameter", values_to="elasticity_coefficient")

for (cattle in c("Cow", "Bull")) {
  if (cattle == "Cow") {
    
    pltMeat <- ggplot(data = sens_cow_[sens_cow_$tissue=="cMeatFat.aSlow",]) + 
      geom_col(aes(parameter, elasticity_coefficient)) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle("Cow - Sensitivity analysis of muscle fat") + 
      xlab("") + 
      ylab("Elasticity coefficient")
    pltLiver <- ggplot(data = sens_cow_[sens_cow_$tissue=="cLiver.aLiver",]) + 
      geom_col(aes(parameter, elasticity_coefficient)) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      ggtitle("Cow - Sensitivity analysis of liver") +
      xlab("") + 
      ylab("Elasticity coefficient")
    fig <- "FigSX1.jpg"
  }
  
  if (cattle == "Bull") {
    pltMeat <- ggplot(data = sens_bull_[sens_bull_$tissue=="cMeatFat.aSlow",]) + 
      geom_col(aes(parameter, elasticity_coefficient)) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      ggtitle("Bull - Sensitivity analysis of muscle fat") +
      xlab("") + 
      ylab("Elasticity coefficient")
    pltLiver <- ggplot(data = sens_bull_[sens_bull_$tissue=="cLiver.aLiver",]) + 
      geom_col(aes(parameter, elasticity_coefficient)) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggtitle("Bull - Sensitivity analysis of liver") +
      xlab("") + 
      ylab("Elasticity coefficient")
    fig <- "FigSX2.jpg"
  }
  
  plt <- grid.arrange(pltMeat,pltLiver,ncol=1)
  ggsave(paste0("figures/", fig),
         plt,
         width=2500,
         height=2000,
         units="px")
  
}

#@@@@@@
# Only plot muscle fat
sens_bull_[sens_bull_$parameter=="pFat",]$parameter <- "pAdipose"
sens_cow_[sens_cow_$parameter=="pFat",]$parameter  <- "pAdipose"
sens_bull_[sens_bull_$parameter=="rVFatBull",]$parameter <- "rVAdiposeBull"
sens_cow_[sens_cow_$parameter=="rVFatCow",]$parameter <- "rVAdiposeCow"


pltMeatBull <- ggplot(data = sens_bull_[sens_bull_$tissue=="cMeatFat.aSlow",]) + 
  geom_col(aes(parameter, elasticity_coefficient)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Bull - Sensitivity analysis of muscle fat") +
  xlab("") + 
  ylab("Elasticity coefficient")
pltMeatCow <- ggplot(data = sens_cow_[sens_cow_$tissue=="cMeatFat.aSlow",]) + 
  geom_col(aes(parameter, elasticity_coefficient)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Cow - Sensitivity analysis of muscle fat") +
  xlab("") + 
  ylab("Elasticity coefficient")

plt <- grid.arrange(pltMeatBull,pltMeatCow,ncol=1)
ggsave(paste0("figures/", fig <- "FigS3.jpg"),
       plt,
       width=2500,
       height=2000,
       units="px")
