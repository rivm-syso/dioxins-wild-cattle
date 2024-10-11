library(ggplot2)

source('model/rodeGeusWeb.R')


intakeGrassBeuningen <- as.data.frame(read.csv("verification/data/beuningen_grass_intake_standard.csv"))
intakeGrassLoevestein <- as.data.frame(read.csv("verification/data/loevestein_grass_intake_standard.csv"))


# Define grass and soil concentrations based on measurements
measuredMonthsGrassBeuningen <- as.integer(format(as.POSIXct(intakeGrassBeuningen$date),"%m"))
measuredMonthsGrassLoevestein <- as.integer(format(as.POSIXct(intakeGrassLoevestein$date),"%m"))

cGrassMin <- mean(c(intakeGrassBeuningen[measuredMonthsGrassBeuningen>3 & measuredMonthsGrassBeuningen<10,]$teq_low, 
                             intakeGrassBeuningen[measuredMonthsGrassBeuningen>3 & measuredMonthsGrassBeuningen<10,]$teq_high))
cGrassMax <- mean(c(intakeGrassBeuningen[measuredMonthsGrassBeuningen<=3 | measuredMonthsGrassBeuningen>=10,]$teq_low, 
                             intakeGrassBeuningen[measuredMonthsGrassBeuningen<=3 | measuredMonthsGrassBeuningen>=10,]$teq_high))

# Correct for moisture in grass (88% d.m -> 100% d.m.)
cGrassBeuningenMin <- cGrassBeuningenMin/0.88
cGrassBeuningenMax <- cGrassBeuningenMax/0.88

# Plot concentrations over time
tSum <- 90
tWin <- 275
kGrass <-  log(2) / 30
tScale <- 365


start <- as.POSIXct("2021-01-01")
cGrassBeuningen <- data.frame(time = seq(1,tScale),
                              date = as.Date(seq(from=start, by=24*60*60, to=(start+ as.difftime(tScale-1, units='days')))),
                              concentration = 0)

for (t in 1:tScale) {
  if (t%%365 < tSum) {
    cGrassBeuningen[cGrassBeuningen$time==t,]$concentration <- pmin(cGrassBeuningenMin * exp(kGrass * (t%%365 + (365 - tWin))), cGrassBeuningenMax)
  } else if ((t%%365 >= tSum) & (t%%365 < tWin)) {
    cGrassBeuningen[cGrassBeuningen$time==t,]$concentration <- pmax(cGrassBeuningenMax * exp(-kGrass * (t%%365 - tSum)), cGrassBeuningenMin)[t%%365 >= tSum & t%%365 < tWin]
  } else if (t%%365 >= tWin) {
    cGrassBeuningen[cGrassBeuningen$time==t,]$concentration <- pmin(cGrassBeuningenMin * exp(kGrass * (t%%365 - tWin)), cGrassBeuningenMax)[t%%365 >= tWin]
    
  }
}

date 
grassPlot <- cGrassBeuningen %>% ggplot(aes(x=date, y=concentration)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%b")) +
  xlab('Date') +
  ylab('Concentration (ng TEQ/kg d.m)') +
  ggtitle('Seasonal variation in grass concentration')
  
ggsave("results/figures/Fig2_grass_conc.jpg", grassPlot, 
       dpi=300,
       height=100,
       width=200,
       units="mm")

