library(ggplot2)
library(gridExtra)

# Script to load and bundle results from calibration
congeners <-  c('2,3,7,8-TCDF', '1,2,3,7,8-PeCDF', '2,3,4,7,8-PeCDF', '1,2,3,4,7,8-HxCDF', 
                '1,2,3,6,7,8-HxCDF', '2,3,4,6,7,8-HxCDF','1,2,3,7,8,9-HxCDF',
                '1,2,3,4,6,7,8-HpCDF', '1,2,3,4,7,8,9-HpCDF','OCDF','2,3,7,8-TCDD',
                '1,2,3,7,8-PeCDD','1,2,3,4,7,8-HxCDD','1,2,3,6,7,8-HxCDD','1,2,3,7,8,9-HxCDD',
                '1,2,3,4,6,7,8-HpCDD','OCDD','PCB 81', 'PCB 77','PCB 126','PCB 169',
                'PCB 123','PCB 118', 'PCB 114','PCB 105','PCB 167',
                'PCB 156','PCB 157', 'PCB 189', 'TEQ2005')


results <- data.frame()
params <- c("pFat", "pLiver", "pSlow", "kMet", "fAbs")

for (c in congeners) {

  theRes <- read.table(paste0("scripts/calibration/fit/", c, "/run1/chains/equal_weighted_post.txt"))
  theRes <- lapply(theRes[-1,], as.numeric)
  
  # Get median, 5th and 95th percentiles of parameters
  theRes <- as.data.frame(theRes)
  theRes <- c(apply(theRes, 2, median, na.rm=T),
           apply(theRes, 2, quantile , probs = 0.05 , na.rm = TRUE ),
           apply(theRes, 2, quantile , probs = 0.95 , na.rm = TRUE ))
  names(theRes) <- c(paste0(params, "_median"), paste0(params, "_p5"), paste0(params, "_p95"))
  
  # Work around to get data frame in correct format. Should be done better
  res<-c()
  theRes <- rbind(res,theRes)
  
  # Add congener information
  theRes <- cbind(as.data.frame(theRes), congener=c)
  theRes <- cbind(theRes, congener_id=nrow(results)+1)
  results <- dplyr::bind_rows(results, theRes)

}

write.csv(results, 'scripts/calibration/results.csv')

#########################
### Visualization #######
#########################
params <- c("pFat", "pLiver", "pSlow", "kMet", "fAbs")
ylabs <- c("Value [-]", "Value [-]", "Value [-]", "Value [/day]", "Value [-]")
i <- 0

ymin <- c(10, 1,  1,  1, 0)
ymax <- c(1000, 1000, 1000, 1000, 1)
plt <- list()
for (param in params) {
  
  i <- i  + 1
  df <- results[c("congener",
                  paste0(param, "_median"),
                  paste0(param, "_p5"),
                  paste0(param, "_p95"))]
  
  
  names(df)<- c("congener", "median", "min","max")
  df[df$congener=="TEQ2005",]$congener <- "Total-TEQ"
  df$congener <- factor(df$congener, levels=unique(df$congener))
  
  if (param == "pFat") param <- "pAdipose"
  plt[[i]] <- ggplot(df, aes(x=congener))+
    geom_linerange(aes(ymin=min,ymax=max),linetype=2,color="blue")+
    geom_point(aes(y=min),size=3,color="red")+
    geom_point(aes(y=max),size=3,color="red")+
    geom_point(aes(y=median),size=5,shape=20,color="black")+
    theme_bw() +
    ylab(ylabs[i]) + 
    theme(axis.text.x = element_text(angle = 90)) + 
    ggtitle(paste("Congener-specific parameter ranges of ", param)) +
    ylim(ymin[i], ymax[i])
  
}
plt_save <- gridExtra::grid.arrange(plt[[1]],plt[[2]],plt[[3]],plt[[4]],plt[[5]], ncol=2, nrow=3)
ggsave("figures/Fig4.jpg",
       plt_save,
       width=35,
       height=35,
       unit="cm")

