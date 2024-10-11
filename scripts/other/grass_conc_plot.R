library(ggplot2)
library(scales)

source('model/model.R')
source('scripts/data_processing/process_calibration_data.R')

cGrassMin <- calibrationData[calibrationData$ID==1 & calibrationData$congener=='TEQ2005',]$cGrassMin
cGrassMax <- calibrationData[calibrationData$ID==1 & calibrationData$congener=='TEQ2005',]$cGrassMax

# Plot concentrations over time
tSum <- 90
tWin <- 275
kGrass <-  log(2) / 30
tScale <- 365

start <- as.POSIXct("2021-01-01")
cGrass <- data.frame(time = seq(1,tScale),
                              date = as.Date(seq(from=start, by=24*60*60, to=(start+ as.difftime(tScale-1, units='days')))),
                              concentration = 0)

for (t in 1:tScale) {
  if (t%%365 < tSum) {
    cGrass[cGrass$time==t,]$concentration <- pmin(cGrassMin * exp(kGrass * (t%%365 + (365 - tWin))), cGrassMax)
  } else if ((t%%365 >= tSum) & (t%%365 < tWin)) {
    cGrass[cGrass$time==t,]$concentration <- pmax(cGrassMax * exp(-kGrass * (t%%365 - tSum)), cGrassMin)[t%%365 >= tSum & t%%365 < tWin]
  } else if (t%%365 >= tWin) {
    cGrass[cGrass$time==t,]$concentration <- pmin(cGrassMin * exp(kGrass * (t%%365 - tWin)), cGrassMax)[t%%365 >= tWin]
  }
}

grassPlot <- cGrass %>% ggplot(aes(x=date, y=concentration)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%b")) +
  xlab('Date') +
  ylab('Concentration (ng TEQ/kg d.m)') +
  ggtitle('Seasonal variation in grass concentration')
  
ggsave("figures/Fig2.jpg", grassPlot, 
       dpi=300,
       height=100,
       width=200,
       units="mm")

