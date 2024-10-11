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

## Code description

This repository consists of three main folders: `model`, `scripts` and `data`. 

In `model`, the main model file is provided. This file contains the parametrisation
as well as the differential equations of the model. `scripts` contains all scripts that were used for calibration and validation of the models. Finally, `data` contains the grass and 
soil concentrations, as well as measured tissue data that were used for model 
calibration and validation. 

## License



