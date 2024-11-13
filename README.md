# dioxins-wild-cattle

This repository contains code that was used to perform the experiments described in the paper "Congener-specific Transfer Modelling of Dioxins and Dioxin-like PCBs in Wild Cattle Grazing on the Dutch Floodplains" by [Minnema et al.](add link) 

## Getting started

The code provided is this repository is coded in R. We recommend installing Rstudio as well.
To get the source code in this repository: 

```
git clone https://github.com/rivm-syso/dioxins-wild-cattle.git
```

The required packages to run the scripts provided in this repository can be viewed 
and installed by running the file `setup.R`

## Prerequisites
The required packages can be installed by running setup.R. This includes the following packages:

R-packages: 
- deSolve; tested with version 1.40
- devtools; tested with version 2.4.5
- dplyr; tested with version 1.1.4
- ggplot2; tested with version 3.5.1
- lifecycle; tested with version 1.0.4
- magrittr; tested with version 2.0.3
- reticulate; tested with version 1.39.0
- tidyverse; tested with version 2.0.0

Python packages (needed for model calibration):
- pillow; tested with version 10.1.0
- scipy; tested with version 1.6.0
- h5py; tested with version 3.10.0
- ultranest; tested with version 3.6.4

## Code description

This repository consists of three main folders: `model`, `scripts` and `data`. 

In `model`, the main model file is provided. This file contains the parametrisation
as well as the differential equations of the model. `scripts` contains all scripts that were used for calibration and validation of the models. Finally, `data` contains the grass and 
soil concentrations, as well as measured tissue data that were used for model 
calibration and validation. 

## Maintanance
The code and data hosted in this repository are solely intended to allow the reproduction
of the results presented in [Minnema et al.](add link). This repository will not be maintained.

## License
EUPL 1.2


