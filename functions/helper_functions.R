#' Helper functions 

formatParamList <- function (inParameterNames, inParameterValues){
  #' Parameter formatting
  #' 
  #' @description function to create a named parameter list
  #' @param inParameterNames the parameter names 
  #' @param inParameterValues numerical values of the model parameters 
  #' @return formatted parameter list
  #' @export
  #' 
  parlist <- list();
  for (i in 1:length(inParameterValues)) {
    parlist[inParameterNames[i]] <- inParameterValues[i];
  }
  return (parlist);
}


param_update <- function(parameters, ...) {
  #' Update parameter value
  #' @description Every parameter value that is in parameters is updated. A warning is 
  #' produced when parameters are supplied that are not in the parameter list
  #' @param parameters Named parameter list.
  #' @param ... Further arguments passed to or from other methods.
  #' @return Updated parameter list.
  
  #' param_update(list(a=1,b=2,c=3,d=4,e=5,f=6), b=4, e=8)
  
  l <- list(...)
  l <- l[!sapply(l, is.null)]
  
  for (param in 1:length(parameters)) {
    if (!(list(NULL) %in% (l[names(parameters[param])]))) {
      parameters[param] <- l[names(parameters[param])]
      l <- within(l, rm(list = names(parameters[param])))
    }
  }
  if (length(l) > 0) warning(paste(names(l), ": not been updated because the parameter(s) do(es) not exist in the model"))
  
  return(parameters)
}


# Helper function
replaceLOQ <- function(x) {
  for (i in 1:length(x)) {
    if (grepl("<",x[i])) {
      x[i] <- as.numeric(gsub("<", "", x[i])) / 2
    }
  }
  return(x)
}
