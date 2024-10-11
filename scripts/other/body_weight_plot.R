#' This script is used to visualize the body weight over time, as well as the 
#' weight of the compartments. 

library(ggplot2)
library(magrittr)

library(MASS) 
library(reshape2) 
library(reshape)

library(gridExtra)

# Clear workspace
rm(list = ls())

# Cow model
source("model/model.R")

parametersCow <- assign_parameters()
parametersCow <- utilities::param_update(parametersCow, 
                                         gender = 'cow',
                                         TSTOP = 14*365,
                                         relFatVariation = 0
                                      )
solutionCow   <- as.data.frame(run_model(parametersCow))
solutionCow <- solutionCow[,c("time", "vFat", "vLiver", "vRich", "vSlow", "vBlood", "wgi")]
colnames(solutionCow)[2] <- "vAdipose"
solutionCow <- melt(solutionCow, id.vars="time")


cowWeights <- data.frame(weight = c(140, 200, 240, 400, 540, 430, 620, 720, 825),
                  age    = c(0.25, 0.5, 0.75, 1.5, 2, 2.5, 3, 5, 5))

plotComposition1 <- ggplot() + 
  geom_area(aes(x=time/365, y=value, fill=variable), data=solutionCow) +
  geom_point(aes(x=age, y=weight), data=cowWeights, size=2) +
  ggtitle("Cows") + 
  ylim(0,1600) +
  ylab('Volume (L)') + 
  xlab("Time (years)")

# Bull model
parametersBull <- assign_parameters()
parametersBull <- calculate_parameters(parametersBull) 
parametersBull <- utilities::param_update(parametersBull, 
                                      gender = 'bull',
                                      TSTOP = 14*365,
                                      relFatVariation = 0)
solutionBull   <- as.data.frame(run_model(parametersBull))
solutionBull <- solutionBull[,c("time",  "vFat", "vLiver", "vRich", "vSlow", "vBlood", "wgi")]
colnames(solutionBull)[2] <- "vAdipose"
solutionBull <- melt(solutionBull, id.vars="time")



bullWeights <- data.frame(weight = c(140, 200, 300, 450, 720, 800, 1000, 1250),
                          age    = c(0.25, 0.5, 0.75, 1.5, 2, 3, 4, 6))

plotComposition2 <- ggplot() + 
  geom_area(aes(x=time/365, y=value, fill=variable), data=solutionBull) +
  geom_point(aes(x=age, y=weight), data=bullWeights, size=2)  +
  ggtitle("Bulls") + 
  ylim(0,1600) + 
  ylab('Volume (L)') + 
  xlab("Time (years)")

# Grid plot
plot_lst <- list(plotComposition1,plotComposition2)
ml1 <- marrangeGrob(plot_lst, nrow = 1, ncol = 2, top=quote("Modelled compartment volumes over time"))
print(ml1)

# Save plot
ggsave("figures/FigS1.jpg", ml1, 
       dpi=300,
       height=100,
       width=300,
       units="mm")

