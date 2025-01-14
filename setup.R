#' File to install all needed packages to run the scripts in this repository
install.packages("lifecycle", force=TRUE)
install.packages("magrittr", force=FALSE)
install.packages("dplyr", force=FALSE)
install.packages("deSolve", force=FALSE)
install.packages("devtools", dependencies=T, force=FALSE)
install.packages("tidyverse", force=FALSE)
install.packages("reticulate", force=FALSE)
install.packages("ggplot2", force=FALSE)

# Installation of (mini-)conda and creating a virtual environment. Only run this if not already done
reticulate::install_miniconda(force=FALSE)
reticulate::conda_create("nested_sampling")

# Install python packages through reticulate and miniconda
reticulate::conda_install("nested_sampling", "pillow", pip=FALSE)
reticulate::conda_install("nested_sampling", "scipy=1.6.0", pip=FALSE)
reticulate::conda_install("nested_sampling", "h5py", pip=FALSE)
reticulate::conda_install("nested_sampling", "ultranest", pip=TRUE)


