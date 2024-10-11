#' Calculate AUC
#' 
#' @description Compute the area under the curve (AUC) based on two sets of values
#' @param x Vector of values.
#' @param y Vector of values with same length as x.
#' @return AUC.
#' @export
AUC <- function(x, y){
  
  auc <- sum(diff(x)*zoo::rollmean(y,2))
  return(auc)
}

#' Plot sensitivity
#' 
#' @description Plot the results of a sensitivity analysis.
#' @param sens Elasticity values calculated with the 'sensitivity_analysis' function.
#' @param comps Model compartments that need to be plotted (optional).
#' @param inFile file name in which the plots should be stored (optional).
#' @export
plot_sensitivity <- function(sens, comps=NULL, inFile=NULL) {
  
  if (is.null(comps)) {
    comps=dimnames(sens)[[1]]
  }
  
  for (comp in comps) {
    par(mar=c(15,  5,5 ,5))
    
    barplot(sens[comp,], ylim=c(-1,1), main=paste0("Sensitivity analysis of ", comp, sep=" "), las=2)
    
    if(!is.null(inFile)){
      bmp(filename = paste0(inFile, comp,".jpg"))
    }
  }
}

#' Sensitivity analysis
#' 
#' @description Main function to perform sensitivity analysis
#' @param model_path path to the model
#' @param parameters named list of all model parameters 
#' @param include_param named list of parameters to be analysed including values
#' @param comps vector of strings containing model compartments that should be included in sensitivity analysis (optional)
#' @param delta fraction of local parameter increase
#' @return elasticity coefficients 
#' @export
sensitivity_analysis <- function(model_path, parameters, include_parameters=NULL, comps=NULL, delta=0.1){  
  
  source(model_path)
  
  # Initialize AUC data frames
  aucStandard <- data.frame()
  aucMax <- data.frame()
  
  simStandard <- as.data.frame(run_model(parameters)) # run_model is a function not defined in this package. Not the nicest to assume user has the function loaded. How to deal with this?
  
  if (is.null(include_parameters)) {
    include_param <- parameters
  }
  else {
    include_param <- parameters[include_parameters]
  }
  
  # Loop over compartments
  for (comp in comps) {
    
    # Compute sensitivity with default parameters
    aucStandard[comp,1] <- AUC(simStandard$time, simStandard[,comp])
    print(paste("Calculating sensitivities for :", comp))
    
    # Loops over model parameters
    for (par in 1:length(include_param)) {
      
      if (!is.numeric(include_param[par][[1]])) {
        warning(paste(names(include_param[par]), " has been skipped since it is not a numeric value"))
        next 
      }
      
      # Increase parameter by 10%
      parStandard <- include_param[names(include_param[par])]
      parMax <- parStandard[[1]]*(delta+1)
      
      # Run simulation with new parameter value
      parameters[names(include_param[par])] <- parMax
      simMax <- as.data.frame(run_model(parameters))
      
      # Calculate new AUC
      aucMax[par, comp] <- AUC(simMax$time, simMax[,comp])
      
      # Reset parameter value
      parameters[names(include_param[par])] <- parStandard
    }
  }
  
  # Initialize matrix to store results
  sens<-matrix(nrow=length(comps), ncol=(length(include_param)))
  dimnames(sens)[[1]] <- comps
  dimnames(sens)[[2]] <- names(include_param)
  
  # Store results
  for (comp in comps) {
    sens[comp,]<- ((aucMax[,comp]-aucStandard[comp,])/aucStandard[comp,])/delta
  }
  
  return(sens)
}





