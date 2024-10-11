#' This script is intended to calibrate the VVH model for cattle in the floodplains
#' Data is used from cattle slaughtered that had been grazing at Loevestein and Beuningen
#' MCMC is applied as the calibration method

# Load packages
library(utilitiesVVH)
library(magrittr)
library(dplyr)

# Load model
source("model/rodeGeusWeb.R")

# Load calibration data
source("calibration/data/process_data.R")

# Get default model parameters
parameters <- assign_parameters()
modelParams <- calculate_parameters(parameters) 
modelParams <- utilitiesVVH::param_update(modelParams,
                                          CINT=7)

### 2,3,7,8-TCDD
### ============================================================================
calibrationData <- calibrationData1[!calibrationData1$ID %in% c(4,5,6),]
calibrationCow <- calibrationData[calibrationData$gender=='cow',]
calibrationBull <- calibrationData[calibrationData$gender=='bull',]

# Parameters to optimize
simParam <- c("rVFatBull", "rVFatCow", "pFat","pLiver", "pRich","pSlow","kMet")
lowerBound <- c(0.02, 0.02, 100, 1,1,1,10)
upperBound <- c(0.15, 0.15, 1000, 100,10,20,60)

modelParams <- calculate_parameters(parameters) 
theUncorrelatedSample <- utilitiesVVH::MCMCSimulation(simParam, 
                                                      modelParams,
                                                      run_model,
                                                      calibrationData[calibrationData$congener == "2,3,7,8-TCDD",],
                                                      inDataErrors = NULL,
                                                      inNoSamples = 5000,
                                                      inNumberOfChains=8,
                                                      inComputeParallel=TRUE,
                                                      inPriors= "uniform",
                                                      inLowerBound=lowerBound,
                                                      inUpperBound=upperBound,
                                                      inMean=NULL,
                                                      inSD=NULL,
                                                      inLinearScaleLLH=FALSE,
                                                      inAlgorithm = 'Adaptive',
                                                      inCompartments = NULL,
                                                      inDiagnosis = TRUE)

theEstimate <- estimateParameterValues(theUncorrelatedSample, simParam)
startSample <- as.data.frame(t(c(280,23,4,8,14,0.25,0.43)))
colnames(startSample)<-simParam
reportEstimateVSStartValue(theEstimate, startSample)

finalParamsBull <- utilities::param_update(modelParams,   
                                           pFat = theEstimate[[1]]@posteriorMedian,
                                           pLiver = theEstimate[[2]]@posteriorMedian,
                                           pRich = theEstimate[[3]]@posteriorMedian,
                                           pSlow = theEstimate[[4]]@posteriorMedian,
                                           kMet = theEstimate[[5]]@posteriorMedian)
sim <- run_model(finalParamsBull)

pltBlood <- ggplot() +
  xlab("Age (years)") +
  ylab("total TEQ concentration (pg TEQ/g blood fat)") +
  #geom_point(data = dataLoevestein[dataLoevestein$comp=="blood",],
  #           aes(x=as.Date(minStart + as.difftime(observedTimes, units='days')), y=conc, color=factor(ID)),size=4) +
  geom_line(data=sim,
            aes(x=time, y=cBloodFat.aBlood), linetype=2) +
  ggtitle("total TEQ concentration in blood fat") 
