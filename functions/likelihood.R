

#' Likelihood simulation
#' 
#' @description calculates the log likelihood of the data given the model parametrisation
#' @param inModelParVals numerical values of the model parameters that are to be varied during optimization
#' @param inParNames the parameter names (in the model code) that correspond to the parameter values
#' @param modelParams list of all (standard) model parameters and values 
#' @param modelRunFunc function to run the model
#' @param inObservations dataframe containing the observational data as well as the dosing information from the experiment
#'                   observations are expected to be formatted as 'obs_' + compartment name. The latter should match the name in the model formulation. 
#'                   the dataframe should at least contain the columns 'id', 'time' and one observations 'obs_<x>'. Columns with updatable model parameters can also be added. 
#' @param inDataErrors errors (uncertainty) on the observations                
#' @param inPerCompartment flag that indicates whether the total likeliHood is calculated (FALSE) or the LL
#'                    is calculated as a list per compartment. The latter option is only useful for diagnostics, not for optimisation
#' @param inLinearScaleLLH determines the formulation of the LLH function. 
#                   if true: the deviation between model and experiment is measured on a linear scale, ie as (d-m)^2
#                   if false: the deviation between model and experiment is measured on a log scale, ie as (log(d)-log(f))^2
#' @param inCompartments list of compartments that should be included in the calculation of the LLH. Allows for the customization of the LLH 
#' @param kLLHScale the constant is used to scale the overall value of the LLH calculated as kLLHScale x LLH. This has an effect on the MCMC and optimization
#             routines as it adjusts the scale of the gradient in the LLH, influencing e.g. the probability of a Markov chain to escape an area of lower local LLH
#             There should be better ways to do this, no doubt. NEEDS IMPROVEMENT
#' @return simulated data likelihood
#' @export
simulateDataLikelihood <- function (inModelParVals, inParNames, modelParams, modelRunFunc, inObservations, inDataErrors = NULL, inPerCompartment=FALSE, inLinearScaleLLH=TRUE, inCompartments = NULL, kLLHScale=100) {
  
  # Check correct format of inObservations
  if (!("ID" %in% colnames(inObservations))) stop("ID is not a column in inObservations")
  if (!any(startsWith(colnames(inObservations), "obs"))) stop("No observations (obs_CMT) provided in inObservations")
  
  # update the model parameters
  parlist <- formatParamList (inParNames, inModelParVals)
  for (p in 1:length(parlist)) modelParams[names(parlist[p])] <- inModelParVals[[p]]
  
  # Create simulation dataframe while updating parameters in inObservations
  theSimulation <- data.frame()
  
  updateParams <- inObservations[!(colnames(inObservations)%in%c('ID', 'time') | startsWith(colnames(inObservations), "obs"))]
  
  # Check that updateParams is not also in inParNames
  if (any(colnames(updateParams) %in% inParNames)) {
    stop("Model parameter(s) passed through inObservations is also present in parameters to be optimized")
  }
  
  for (i in unique(inObservations$ID)) {
    if (ncol(updateParams)>0) {
      if (length(unique(inObservations[inObservations$ID==i,colnames(updateParams)][1]))>1) {
        stop("Various parameter values are provided for the same ID")}
      else {  
        modelParams[colnames(updateParams)] <- inObservations[inObservations$ID==i,colnames(updateParams)][1,]
      }
    }
    
    theSimulation <- rbind(theSimulation,
                           cbind(data.frame(ID=i),
                                 as.data.frame(modelRunFunc(modelParams)) %>% dplyr::filter(time %in% inObservations[inObservations$ID==i,]$time)))
    
  }
  
  # read simulation result into a data frame and determine the compartments to be included in the the loglikehood calculation, first removing the non-obervational columns from the data 
  theObservationCompartments <- colnames(inObservations[!colnames(inObservations)%in%c('ID', 'time', 'evid', 'amt', 'rate', 'cmt', 'BW')]);
  if (is.null(inCompartments)) {
    # include all simulated compartments. These are the columns left after removing ID and time columns
    theSimulationCompartments  <- colnames(theSimulation[!colnames(theSimulation)%in%c('ID', 'time')]);
  } else {
    theSimulationCompartments <- inCompartments;
  }
  
  theLLH <- 0;
  
  for (compartment in theSimulationCompartments) {
    # infer compartment name from observed component name
    observationComp <- paste('obs_', compartment, sep ="" );
    
    if (observationComp %in% theObservationCompartments) {
      observations <- inObservations[,observationComp];
      compSimulation <- theSimulation[,compartment];
      
      # get the observational errors for the compartment, to pass to the likelihood calculation
      # note that this works also if the errors == NULL
      theErrors <- inDataErrors[,observationComp];
      
      if (inLinearScaleLLH) {
        # remove non-observations from the simulation and observation data
        compSimulation <- compSimulation[!is.na(observations)];
        observations   <- observations[!is.na(observations)];
        theLLH <- theLLH + logLikeliHoodLinearScale(observations, compSimulation, inDataErrors = theErrors);
      } else {
        # remove the non-observations (note that a value of 1 will make terms drop out of the likelihood sum)
        compSimulation <- compSimulation[!is.na(observations)];
        observations   <- observations[!is.na(observations)];
        # remove the <= zeros
        compSimulation[!(observations>0)]     <- 1;
        observations[!(observations>0)]     <- 1;
        compSimulation[!(compSimulation>0)] <- 1;
        theLLH <- theLLH + logLikeliHoodLogScale (observations, compSimulation, inDataErrors = theErrors);  
        
        if (theLLH < 0) {
          print("STOP")
        }
      }
    }
  }
  
  theDataLikelihood <- theLLH/length(theObservationCompartments);
  theDataLikelihood <- kLLHScale * theDataLikelihood;
  return (-theDataLikelihood);
}

# helper routines called by simulateDataLikelihood()
logLikeliHoodLinearScale <- function (inCompObservations, inCompSimulation, inDataErrors = NULL, inScaled = TRUE) {
  # observations are expected to be on the same time points as the simulated data
  # compartments in the observation are assumed to be named 'obs_CMT' where 'CMT' is the
  # compartment name. Then, matching observation and simulated points are identified and the normalized
  # setting the list inCompartments allows one to limit the number of compartments, accounting only for selected compartments
  # if inCompartments == NULL all the observational compartments will be included in calculating the likeliHood
  # likelihood is calculated as the exponent of -(sum of squares of difference between model and data) is calculated and scaled to the observational datum in absence of error information
  # data errors are assumed to be the sd in the observational data
  theScale <- 1;
  if (!is.null(inDataErrors)) {
    theScale <- inDataErrors;
    
  } else if (inScaled) {
    theScale <- inCompObservations;
    # replace 0's with 1
    theScale[!(theScale>0)] <- 1;
  }
  return (sum(((inCompSimulation - inCompObservations)/theScale)^2)/length(inCompSimulation));
  
}

logLikeliHoodLogScale <- function (inCompObservations, inCompSimulation, inDataErrors = NULL) {
  # observations are expected to be on the same time points as the simulated data
  # compartments in the observation are assumed to be named 'obs_CMT' where 'CMT' is the
  # compartment name. Then, matching observation and simulated points are identified and the normalized 
  # setting the list inCompartments allows one to limit the number of compartments, accounting only for selected compartments
  # if inCompartments == NULL all the observational compartments will be included in calculating the likeliHood
  # likelihood is calculated as the exponent of -(sum of squares of difference between model and data) is calculated 
  # if data errors are passed for heteroscadacitic scaling, these are assumed to represent the sd in the log of the data
  theScale <- 1;
  if (!is.null(inDataErrors)) {
    theScale <- inDataErrors;
  }
  
  return(sum(((log(inCompSimulation) - log(inCompObservations))/theScale)^2)/length(inCompObservations));
}
