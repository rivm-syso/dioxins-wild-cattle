#' script intended to read and pre-process the dioxin and dl-pcb data measurement in 
#' animal tissues, grass and soil. This data is used for the congener-specific 
#' calibration of the RodeGeus transfer model. 

library(readxl)
library(magrittr)
library(tidyr)
library(dplyr)
library(plyr)

# Integration step required
f.preprocess_calibration <- function() {
  
  # Load tefs
  tef <- read.csv("scripts/validation/tef_values.csv")
  
  CINT = 1
  
  congeners <- c('2,3,7,8-TCDF', '1,2,3,7,8-PeCDF', '2,3,4,7,8-PeCDF', '1,2,3,4,7,8-HxCDF', 
                 '1,2,3,6,7,8-HxCDF', '2,3,4,6,7,8-HxCDF','1,2,3,7,8,9-HxCDF',
                 '1,2,3,4,6,7,8-HpCDF', '1,2,3,4,7,8,9-HpCDF','OCDF','2,3,7,8-TCDD',
                 '1,2,3,7,8-PeCDD','1,2,3,4,7,8-HxCDD','1,2,3,6,7,8-HxCDD','1,2,3,7,8,9-HxCDD',
                 '1,2,3,4,6,7,8-HpCDD','OCDD','PCB 81', 'PCB 77','PCB 126','PCB 169',
                 'PCB 123','PCB 118', 'PCB 114','PCB 105','PCB 167','PCB 156',
                 'PCB 157', 'PCB 189')
  
  # Helper function
  replaceLOQ <- function(x) {
    for (i in 1:length(x)) {
      if (grepl("<",x[i])) {
        x[i] <- as.numeric(gsub("<", "", x[i])) / 2
      }
    }
    return(x)
  }
  
  ## =============== Loevestein ================================= ##
  
                 
  # Read first files for Loevestein with blood and tissue data
  Loevestein1 <- as.data.frame(read_xlsx("data/Loevestein2.xlsx", sheet='bloed en vet'))
  Loevestein1 <- Loevestein1[Loevestein1$...1 %in% congeners,] 
  
  # Process first 'batch' of data, for the first blood samples
  Loevestein1tmp <- Loevestein1[c(1,5:9)]
  colnames(Loevestein1tmp)[1] <- 'congener'
  colnames(Loevestein1tmp)[2:6] <- c('7', '8', '9', '10', '11')
  LoevesteinData <- pivot_longer(Loevestein1tmp, 
                                 colnames(Loevestein1tmp)[2:6],
                                 names_to = 'ID',
                                 values_to = 'obs_cBloodFat.aBlood') %>% mutate(Date = "2021-03-26")
  
  # Process second 'batch' of data, for the second blood samples
  Loevestein1tmp <- Loevestein1[c(1,11:15)]
  colnames(Loevestein1tmp)[1] <- 'congener'
  colnames(Loevestein1tmp)[2:6] <- c('7', '8', '9', '10', '11')
  LoevesteinData <- rbind(LoevesteinData, 
                          pivot_longer(Loevestein1tmp, 
                                       colnames(Loevestein1tmp)[2:6],
                                       names_to = 'ID',
                                       values_to = 'obs_cBloodFat.aBlood') %>% mutate(Date = "2021-06-09"))
  
  # Process third 'batch' of data, for the third blood samples
  Loevestein1tmp <- Loevestein1[c(1,17:21)]
  colnames(Loevestein1tmp)[1] <- 'congener'
  colnames(Loevestein1tmp)[2:6] <- c('7', '8', '9', '10', '11')
  LoevesteinData <- rbind(LoevesteinData, 
                          pivot_longer(Loevestein1tmp, 
                                       colnames(Loevestein1tmp)[2:6],
                                       names_to = 'ID',
                                       values_to = 'obs_cBloodFat.aBlood') %>% mutate(Date = "2021-11-04"))
  
  # Process fourth 'batch' of data, for the kidney fat samples
  Loevestein1tmp <- Loevestein1[c(1,23:27)]
  colnames(Loevestein1tmp)[1] <- 'congener'
  colnames(Loevestein1tmp)[2:6] <- c('7', '8', '9', '10', '11')
  LoevesteinData <- merge(LoevesteinData, 
                          pivot_longer(Loevestein1tmp, 
                                       colnames(Loevestein1tmp)[2:6],
                                       names_to = 'ID',
                                       values_to = 'obs_cMeatFat.aSlow') %>% mutate(Date = "2021-11-04"),
                          all=TRUE)
  
  # Process fifth 'batch' of data, for the liver samples
  Loevestein1tmp <- Loevestein1[c(1,30:34)]
  colnames(Loevestein1tmp)[1] <- 'congener'
  colnames(Loevestein1tmp)[2:6] <- c('7', '8', '9', '10', '11')
  LoevesteinData <- merge(LoevesteinData, 
                          pivot_longer(Loevestein1tmp, 
                                       colnames(Loevestein1tmp)[2:6],
                                       names_to = 'ID',
                                       values_to = 'obs_cLiver.aLiver') %>% mutate(Date = "2021-11-04"),
                          all=TRUE)
  
  # Loevestein kidney samples (IDs 15,16,17)
  Loevestein1tmp <- Loevestein1[c(1,37:39)]
  colnames(Loevestein1tmp)[1] <- 'congener'
  colnames(Loevestein1tmp)[2:4] <- c('12', '13', '14')
  LoevesteinData <- merge(LoevesteinData, 
                          pivot_longer(Loevestein1tmp, 
                                       colnames(Loevestein1tmp)[2:4],
                                       names_to = 'ID',
                                       values_to = 'obs_cMeatFat.aSlow') %>% mutate(Date = "2021-11-08"),
                          all=TRUE)
  
  
  # Loevestein tissue samples (IDs 7,8,9)
  Loevestein1Tissues <- as.data.frame(read_xlsx("data/Loevestein2.xlsx", sheet='Loevestein stieren'))
  Loevestein1Tissues <- Loevestein1Tissues[Loevestein1Tissues$`Tabel 3A:` %in% congeners,] 
  
  Loevestein1Tissuestmp <- Loevestein1Tissues[c(1,8,9,12)]
  colnames(Loevestein1Tissuestmp)[1] <- 'congener'
  colnames(Loevestein1Tissuestmp)[2:4] <- c('obs_cLiver.aLiver', 'obs_cMeatFat.aSlow', 'obs_cBloodFat.aBlood')
  Loevestein1Tissuestmp <- Loevestein1Tissuestmp %>% mutate(Date = "2021-03-26", ID = "4")
  
  LoevesteinData <- merge(LoevesteinData, Loevestein1Tissuestmp, all=TRUE)
  
  Loevestein1Tissuestmp <- Loevestein1Tissues[c(1,14,15,18)]
  colnames(Loevestein1Tissuestmp)[1] <- 'congener'
  colnames(Loevestein1Tissuestmp)[2:4] <- c('obs_cLiver.aLiver', 'obs_cMeatFat.aSlow', 'obs_cBloodFat.aBlood')
  Loevestein1Tissuestmp <- Loevestein1Tissuestmp %>% mutate(Date = "2021-03-26", ID = "5")
  
  LoevesteinData <- merge(LoevesteinData, Loevestein1Tissuestmp, all=TRUE)
  
  Loevestein1Tissuestmp <- Loevestein1Tissues[c(1,20,21,24)]
  colnames(Loevestein1Tissuestmp)[1] <- 'congener'
  colnames(Loevestein1Tissuestmp)[2:4] <- c('obs_cLiver.aLiver', 'obs_cMeatFat.aSlow', 'obs_cBloodFat.aBlood')
  Loevestein1Tissuestmp <- Loevestein1Tissuestmp %>% mutate(Date = "2021-03-26", ID = "6")
  
  LoevesteinData <- merge(LoevesteinData, Loevestein1Tissuestmp, all=TRUE)
  
  
  
  ## =============== Beuningen ================================= ##
  
  BeuningenTissues1 <- as.data.frame(read_xlsx("data/Beuningen1.xlsx", sheet='Lever en vet'))
  BeuningenTissues1 <- BeuningenTissues1[1:80,] # Remove double values in excel
  BeuningenTissues1 <- BeuningenTissues1[BeuningenTissues1$`Tabel 3A:` %in% congeners,] 
  
  BeuningenTissues1tmp <- BeuningenTissues1[c(1,7:9)]
  colnames(BeuningenTissues1tmp)[1] <- 'congener'
  colnames(BeuningenTissues1tmp)[2:4] <- c('1', '2', '3')
  BeuningenData <- pivot_longer(BeuningenTissues1tmp, 
               colnames(BeuningenTissues1tmp)[2:4],
               names_to = 'ID',
               values_to = 'obs_cLiver.aLiver') 
  
  BeuningenTissues1tmp <- BeuningenTissues1[c(1,12:14)]
  colnames(BeuningenTissues1tmp)[1] <- 'congener'
  colnames(BeuningenTissues1tmp)[2:4] <- c('1', '2', '3')
  BeuningenData <- merge(BeuningenData, 
                         pivot_longer(BeuningenTissues1tmp, 
                                      colnames(BeuningenTissues1tmp)[2:4],
                                      names_to = 'ID',
                                      values_to = 'obs_cMeatFat.aSlow'),
                         all = T)
  
  BeuningenData <- BeuningenData %>% mutate(Date = "2021-02-02")

  
  
  ## ========================= Read intake data ================================= #

  
  intakeData <- as.data.frame(read_xlsx("data/Grasgrondvegetatie.xlsx", sheet='Gras Grond'))
  
  # Grass intake from Beuningen (used for both Beuningen and Loevestein)
  BeuningenGrass <- intakeData[intakeData$`Tabel 3A:` %in% congeners,c(1,5:29)] 
  colnames(BeuningenGrass)[1] <- 'congener'
  colnames(BeuningenGrass)[2:26] <- c('Winter', 'Winter', 'Control','Flood', 
                                      'Control', 'Summer', 'Summer', 'Summer',
                                      'Flood','Summer','Summer', 'Summer',
                                      'Winter','Winter','Control','Flood',
                                      'Flood','Control','Flood','Summer',
                                      'Summer', 'Summer','Winter','Winter',
                                      'Winter')
  
  BeuningenGrass <- pivot_longer(BeuningenGrass, 
                                 colnames(BeuningenGrass)[2:26],
                                 names_to = 'Period',
                                 values_to = 'conc')
  
  # Soil intake from Beuningen 
  BeuningenSoil <- intakeData[intakeData$`Tabel 3A:` %in% congeners,c(1,31:34,36:42)] 
  colnames(BeuningenSoil)[1] <- 'congener'
  colnames(BeuningenSoil)[2:12] <- rep('Soil',11)
  
  BeuningenSoil <- pivot_longer(BeuningenSoil, 
                                 colnames(BeuningenSoil)[2:12],
                                 names_to = 'Period',
                                 values_to = 'conc')
  
  # Soil intake from Loevestein 
  LoevesteinSoil <- intakeData[intakeData$`Tabel 3A:` %in% congeners,c(1,44:47)] 
  colnames(LoevesteinSoil)[1] <- 'congener'
  colnames(LoevesteinSoil)[2:5] <- rep('Soil',4)
  
  LoevesteinSoil <- pivot_longer(LoevesteinSoil, 
                                colnames(LoevesteinSoil)[2:5],
                                names_to = 'Period',
                                values_to = 'conc')
  
  # Summarise data
  BeuningenGrass[,"conc"] <- lapply(BeuningenGrass[,"conc"], replaceLOQ)
  BeuningenSoil[,"conc"] <- lapply(BeuningenSoil[,"conc"], replaceLOQ)
  LoevesteinSoil[,"conc"] <- lapply(LoevesteinSoil[,"conc"], replaceLOQ)
  
  grassSummary <- ddply(BeuningenGrass, c("congener", "Period"), summarise, mean = mean(as.numeric(conc), na.rm=T))
  soilSummaryBeuningen <-  ddply(BeuningenSoil, c("congener", "Period"), summarise, mean = mean(as.numeric(conc), na.rm=T))
  soilSummaryLoevestein <-  ddply(LoevesteinSoil, c("congener", "Period"), summarise, mean = mean(as.numeric(conc), na.rm=T))
  
  # Process data in data table
  LoevesteinData <- LoevesteinData %>% mutate(cGrassMax = join(data.frame(congener = congener),
                                                                 grassSummary[grassSummary$Period == "Winter", ])$mean/0.88,
                                                cGrassMin = join(data.frame(congener = congener),
                                                                 grassSummary[grassSummary$Period == "Summer", ])$mean/0.88,
                                                cGrassClean = 0,
                                                cSoil = join(data.frame(congener = congener),
                                                             soilSummaryLoevestein[soilSummaryLoevestein$Period == "Soil", ])$mean)
  
  
  BeuningenData <- BeuningenData %>% mutate(cGrassMax = join(data.frame(congener = congener),
                                                                 grassSummary[grassSummary$Period == "Winter", ])$mean/0.88,
                                                cGrassMin = join(data.frame(congener = congener),
                                                                 grassSummary[grassSummary$Period == "Summer", ])$mean/0.88,
                                                cGrassClean = 0,
                                                cSoil = join(data.frame(congener = congener),
                                                             soilSummaryBeuningen[soilSummaryBeuningen$Period == "Soil", ])$mean)
  
  
  
  ## =============== Data compilation ================================= ##
  tissueData <- merge(LoevesteinData, BeuningenData, all=TRUE)
  tissueData <- tissueData[with(tissueData, order(ID)),]
  
  ### ===================== Add simulation parameters ================= ###
  
  # Birth days of the cattle ordered by ID (1:17) Animals 4,5, and 6 are excluded
  birth_day <- c("2019-11-12", "2005-06-18", "2017-06-09", # Beuningen ID 1:3
                 "2018-12-30", "2019-04-12", "2018-07-29", # Loevestein ID 7:9
               "2020-03-11", "2020-03-12", "2020-04-01", "2020-06-05", "2020-03-11", # Loevestein ID 10:14
               "2019-10-09", "2020-03-12","2017-03-08") # Loevestein ID 15:17
  t_stop <- c("2021-02-02", "2021-02-02", "2021-02-02", # Beuningen ID 1:3
              "2021-03-26", "2021-03-26", "2021-03-26", # Loevestein ID 7:9
              "2021-11-04", "2021-11-04", "2021-11-04", "2021-11-04", "2021-11-04", # Loevestein ID 10:14
              "2021-11-08", "2021-11-08", "2021-11-08") # Loevestein ID 15:17
  t_clean <- c("2021-02-02", "2021-02-02", "2021-02-02", # Beuningen ID 1:3
              "2021-03-26", "2021-03-26", "2021-03-26", # Loevestein ID 7:9
              "2021-04-01", "2021-04-01", "2021-04-01", "2021-04-01", "2021-04-01", # Loevestein ID 10:14
              "2021-11-08", "2021-11-08", "2021-11-08") # Loevestein ID 15:17
  gender <- c(rep("cow",3),rep("bull",10), "cow")
  
  # Add cattle birth days
  calibrationData <- tissueData %>% mutate(birthDay = birth_day[as.numeric(ID)],
                                           TSTOP = t_stop[as.numeric(ID)],
                                           tClean = t_clean[as.numeric(ID)],
                                           simClean = TRUE,
                                           gender = gender[as.numeric(ID)])
  
  # Add time to measurements from start of birth year (= start of simulation)
  calibrationData <- calibrationData %>% mutate(time = birthDay %>% 
                                                  sapply(strsplit, split="-") %>% 
                                                  sapply("[[", 1) %>% 
                                                  sapply(paste0, "-01-01") %>%
                                                  difftime(Date) %>%
                                                  as.integer() %>%
                                                  abs())
  
  # Add time to stop simulation from start of birth year (= start of simulation)
  calibrationData <- calibrationData %>% mutate(TSTOP = birthDay %>% 
                                                  sapply(strsplit, split="-") %>% 
                                                  sapply("[[", 1) %>% 
                                                  sapply(paste0, "-01-01") %>%
                                                  difftime(t_stop[as.numeric(ID)]) %>%
                                                  as.integer() %>%
                                                  abs())
  
  # Add time to stop contaminated feed from start of birth year (= start of simulation)
  calibrationData <- calibrationData %>% mutate(tClean = birthDay %>%
                                                  sapply(strsplit, split="-") %>% 
                                                  sapply("[[", 1) %>% 
                                                  sapply(paste0, "-01-01") %>%
                                                  difftime(t_clean[as.numeric(ID)]) %>%
                                                  as.integer() %>%
                                                  abs())
  
  # Add flood timing from start of birth year (=start of simulation)
  calibrationData <- calibrationData %>% mutate(simFlood = TRUE,
                                                tFlood = birthDay %>%
                                                  sapply(strsplit, split="-") %>% 
                                                  sapply("[[", 1) %>% 
                                                  sapply(paste0, "-01-01") %>%
                                                  difftime("2021-02-01") %>%
                                                  as.integer() %>%
                                                  abs(),
                                                dFlood = 28)

  
  # Set every entry below LOQ equal to zero
  cols <- c("obs_cLiver.aLiver", "obs_cBloodFat.aBlood", "obs_cMeatFat.aSlow", "cGrassMax", "cGrassMin")
  calibrationData[cols] <- lapply(calibrationData[,cols], replaceLOQ)
  calibrationData[cols] <- lapply(calibrationData[,cols], as.numeric)
  
  # If cGrassMin > cGrassMax, set them both equal to their mean
  replace_grass <- unique(calibrationData[calibrationData$cGrassMin > calibrationData$cGrassMax,]$congener)
  for (c in replace_grass) {
    cmean <- (calibrationData[calibrationData$congener==c,]$cGrassMin + calibrationData[calibrationData$congener==c,]$cGrassMax) / 2
    calibrationData[calibrationData$congener==c,]$cGrassMin <- cmean
    calibrationData[calibrationData$congener==c,]$cGrassMax <- cmean
  }
  
  
  # Round given time points for CINT
  tcols <- c("TSTOP", "tClean", "time", "tFlood")
  calibrationData[tcols] <- lapply(calibrationData[tcols], round_any, CINT) 
  
  
  #calibrationData[calibrationData$congener == 'WHO2005-PCDD/F-PCB-TEQ (ub)',]$congener <- 'TEQ2005'
  getTEQ <- function(x,tef,year="2005") {
    names(tef)[1] <- "congener"
    if(year=="2005") names(tef)[2]<-"tef"
    else names(tef)[3]<-"tef"
    names(x)[2]<-"obs"
    x <- x %>% merge(tef) %>% mutate(teq = obs*tef)
    return(sum(x$teq))
  }
    
  # Calculate TEQ2005 and TEQ2022
  TEQ <- data.frame()
  for (id in unique(calibrationData$ID)) {
    dat <- calibrationData[calibrationData$ID==id,]
    
    dates <- unique(dat$Date)
    for (date in 1:length(dates)) {

      TEQ2005 <- data.frame(congener='TEQ2005',
                            ID=id,
                            Date=dates[date],
                            obs_cMeatFat.aSlow=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cMeatFat.aSlow")], tef, year="2005"),
                            obs_cLiver.aLiver=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cLiver.aLiver")], tef, year="2005"),
                            cGrassMax=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMax")], tef, year="2005"),
                            cGrassMin=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMin")], tef, year="2005"),
                            cGrassClean=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassClean")], tef, year="2005"),
                            cSoil=getTEQ(dat[dat$Date==dates[date],c("congener", "cSoil")], tef, year="2005"),
                            obs_cBloodFat.aBlood=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cBloodFat.aBlood")], tef, year="2005"),
                            birthDay=dat$birthDay[date],
                            TSTOP=dat$TSTOP[date],
                            tClean=dat$tClean[date],
                            simClean=dat$simClean[date],
                            gender=dat$gender[date],
                            time=dat$time[date],
                            simFlood=dat$simFlood[date],
                            tFlood=dat$tFlood[date],
                            dFlood=dat$dFlood[date])
      TEQ2022 <- data.frame(congener='TEQ2022',
                            ID=id,
                            Date=dates[date],
                            obs_cMeatFat.aSlow=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cMeatFat.aSlow")], tef, year="2022"),
                            obs_cLiver.aLiver=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cLiver.aLiver")], tef, year="2022"),
                            cGrassMax=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMax")], tef, year="2022"),
                            cGrassMin=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMin")], tef, year="2022"),
                            cGrassClean=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassClean")], tef, year="2022"),
                            cSoil=getTEQ(dat[dat$Date==dates[date],c("congener", "cSoil")], tef, year="2022"),
                            obs_cBloodFat.aBlood=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cBloodFat.aBlood")], tef, year="2022"),
                            birthDay=dat$birthDay[date],
                            TSTOP=dat$TSTOP[date],
                            tClean=dat$tClean[date],
                            simClean=dat$simClean[date],
                            gender=dat$gender[date],
                            time=dat$time[date],
                            simFlood=dat$simFlood[date],
                            tFlood=dat$tFlood[date],
                            dFlood=dat$dFlood[date])
      
      TEQ <- rbind(TEQ, rbind(TEQ2005,TEQ2022))
      
    }
  } 

  calibrationData <- rbind(calibrationData, TEQ)
  return(calibrationData)
}
  
calibrationData <- f.preprocess_calibration()
