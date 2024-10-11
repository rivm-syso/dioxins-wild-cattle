#' This script is intended to calibrate the VVH model for cattle in the floodplains
#' Data is used from cattle slaughtered that had been grazing at Loevestein and Beuningen
#' Nested sampling is applied as the calibration method

#' Make sure to run setup.R before running this script

# Load packages
library(magrittr)
library(dplyr)
library(reticulate)

# Load python package ultranest
reticulate::use_condaenv("nested_sampling")
un <- reticulate::import("ultranest")

# Load model
source("model/model.R")

# Load calibration data
source("scripts/data_processing/process_calibration_data.R")

## ====================================================================== #
## =========================== Script =================================== #
## ====================================================================== #

# Get default model parameters
parameters <- assign_parameters()

# Change time scale since simulations would take way too long otherwise
unit = "months" # To speed up computations, chosen time unit was months
parameters <- utilitiesVVH::param_update(parameters,
                                         unit=unit,
                                         tStartGrassIntake=0) 

# Account for observation time unit by dividing by 28 (assuming that month=28 days).
# Note that other timing parameters (e.g., tFlood, tClean) do not need to be adjusted,
# since that is already done in model.R
calibrationData$time <- round(calibrationData$time / 28 )

# Define congeners to optimize
congeners <- unique(calibrationData$congener)
congeners <- congeners[c(1:30)]

# Parameters to optimize
parm.names <- c("pFat","pLiver","pSlow","kMet", "fAbs")
lowerBound <- c(10, 1,  1,  1, 0)
upperBound <- c(1000, 1000, 1000, 1000, 1)

# Loop over congeners (including TEQ2005)
for (c in congeners) {
  
  tStart <- Sys.time()
  
  # Likelihood definition
  Model <- function(parms) { 
    LLH <- utilitiesVVH::simulateDataLikelihood(inModelParVals = parms,
                           inParNames = parm.names,
                           parameters,
                           run_model,
                           calibrationData[calibrationData$congener == c,],
                           inLinearScaleLLH = FALSE,
                           inCompartments = NULL)
    return(LLH)
  }
  
  # Transformation definition
  Transform <- function(q) {
    
    # Uniform prior distribution
    parm <- numeric(length(q))
    for (i in 1:length(q)) {
      parm[i] <- q[i]*(upperBound[i]-lowerBound[i]) + lowerBound[i]
    }
    return(parm)
    
  }
  
  # ultranest sampler
  sampler <- un$ReactiveNestedSampler(parm.names, Model, Transform, log_dir=paste0('scripts/calibration/fit/',c))
  results <- sampler$run(min_num_live_points = as.integer(400))
  sampler$print_results()
  
  print(paste0("Calibration took: ", Sys.time() - tStart))
  
}
