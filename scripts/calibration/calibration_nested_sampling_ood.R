#' This script is intended to calibrate the VVH model for cattle in the floodplains
#' Data is used from cattle slaughtered that had been grazing at Loevestein and Beuningen
#' Nested sampling is applied as the calibration method

#' Make sure to run setup.R before running this script
#' 

library(magrittr)
library(dplyr)

## ====================================================================== #
## =========================== Script =================================== #
## ====================================================================== #

# Load python package ultranest
library(reticulate)
use_virtualenv("r-reticulate")

if (F) {
  virtualenv_install("r-reticulate", "pillow")
  virtualenv_install("r-reticulate", "scipy")
  virtualenv_install("r-reticulate", "h5py")
  virtualenv_install("r-reticulate", "ultranest")
}

un <- reticulate::import("ultranest")

# Load model
source("model/rodeGeusWeb.R")

# Load calibration data
source("scripts/data_processing/process_calibration_data_v2.R")

# Get default model parameters
parameters <- assign_parameters()
#parameters <- calculate_parameters(parameters) 

# Change time scale since simulations would take way too long otherwise
unit = "months"
parameters <- utilitiesVVH::param_update(parameters,
                                         unit=unit,
                                         tStartGrassIntake=0) 

if (unit == "weeks") calibrationData$time <- round(calibrationData$time / 7 )
if (unit == "months") calibrationData$time <- round(calibrationData$time / 28 )

# Define congeners to optimize
congeners <- unique(calibrationData$congener)
congeners <- congeners[4]

# Parameters to optimize
parm.names <- c("pFat","pLiver","pSlow","kMet", "fAbs")
lowerBound <- c(10, 1,  1,  1, 0)
upperBound <- c(1000, 1000, 1000, 1000, 1)

for (c in congeners) {
  
  tStart <- Sys.time()
  # Likelihood definition
  Model <- function(parms) { #Likelihood function
    LLH <- utilitiesVVH::simulateDataLikelihood(inModelParVals = parms,
                           inParNames = parm.names,
                           parameters,
                           run_model,
                           calibrationData[calibrationData$congener == c,],
                           inLinearScaleLLH = FALSE,
                           inCompartments = NULL)
    return(LLH)
  }
  
  Transform <- function(q) {
    
    # Uniform prior distribution
    parm <- numeric(length(q))
    for (i in 1:length(q)) {
      parm[i] <- q[i]*(upperBound[i]-lowerBound[i]) + lowerBound[i]
    }
    return(parm)
    
  }
  
  sampler <- un$ReactiveNestedSampler(parm.names, Model, Transform, log_dir=paste0('scripts/calibration/fit_updated_intake/',c, '/run1'),resume=TRUE)
  results <- sampler$run(min_num_live_points = as.integer(400))
  sampler$print_results()
  
  print(paste0("Calibration took: ", Sys.time() - tStart))
  
}
