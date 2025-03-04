#' script intended to read and pre-process the dioxin and dl-pcb data measurement in 
#' animal tissues, grass and soil. This data is used for the congener-specific 
#' verification of the RodeGeus transfer model. 

library(readxl)
library(tidyr)
library(plyr)
library(dplyr)


# Integration step required
f.preprocess_verification <- function() {
  
  CINT = 7 # days
  tef <- read.csv("scripts/validation/tef_values.csv")
  
  congeners <- c('2,3,7,8-TCDF', '1,2,3,7,8-PeCDF', '2,3,4,7,8-PeCDF', '1,2,3,4,7,8-HxCDF', 
                 '1,2,3,6,7,8-HxCDF', '2,3,4,6,7,8-HxCDF','1,2,3,7,8,9-HxCDF',
                 '1,2,3,4,6,7,8-HpCDF', '1,2,3,4,7,8,9-HpCDF','OCDF','2,3,7,8-TCDD',
                 '1,2,3,7,8-PeCDD','1,2,3,4,7,8-HxCDD','1,2,3,6,7,8-HxCDD','1,2,3,7,8,9-HxCDD',
                 '1,2,3,4,6,7,8-HpCDD','OCDD','PCB 81', 'PCB 77','PCB 126','PCB 169',
                 'PCB 123','PCB 118', 'PCB 114','PCB 105','PCB 167','PCB 156',
                 'PCB 157', 'PCB 189')
  
  # Read first files for beuningen with kidney fat
  Data <- as.data.frame(read_xlsx("data/Najaar2023.xlsx", sheet='dioxines PCBs'))
  Data <- Data[Data$...1 %in% congeners,] 

  # Process data Loevestein
  Loevestein <- Data[c(1,7:13)]
  colnames(Loevestein)[1] <- 'congener'
  colnames(Loevestein)[2:8] <- c('22', '23', '24', '25', '26', '27', '28')
  LoevesteinData <- pivot_longer(Loevestein, 
                                colnames(Loevestein)[2:8],
                                names_to = 'ID',
                                values_to = 'obs_cMeatFat.aSlow') %>% mutate(Date = "2023-10-31")
    
  # Process data Beuningen
  Beuningen <- Data[c(1,15:21)]
  colnames(Beuningen)[1] <- 'congener'
  colnames(Beuningen)[2:8] <- c('15', '16', '17', '18', '19', '20', '21')

  BeuningenData <- pivot_longer(Beuningen, 
                                colnames(Beuningen)[2:8],
                                names_to = 'ID',
                                values_to = 'obs_cMeatFat.aSlow') %>% mutate(Date = "2023-11-07")
  
  verificationData <- rbind(BeuningenData, LoevesteinData)
  
  
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
  
  # Soil intake from Beuningen 
  BeuningenSoil <- intakeData[intakeData$`Tabel 3A:` %in% congeners,c(1,31:34,36:42)] 
  colnames(BeuningenSoil)[1] <- 'congener'
  colnames(BeuningenSoil)[2:12] <- rep('Soil',11)
  
  # Soil intake from Loevestein 
  LoevesteinSoil <- intakeData[intakeData$`Tabel 3A:` %in% congeners,c(1,44:47)] 
  colnames(LoevesteinSoil)[1] <- 'congener'
  colnames(LoevesteinSoil)[2:5] <- rep('Soil',4)
  
  BeuningenGrass <- pivot_longer(BeuningenGrass, 
                                 colnames(BeuningenGrass)[2:26],
                                 names_to = 'Period',
                                 values_to = 'conc')
  BeuningenSoil <- pivot_longer(BeuningenSoil, 
                                colnames(BeuningenSoil)[2:12],
                                names_to = 'Period',
                                values_to = 'conc')
  LoevesteinSoil <- pivot_longer(LoevesteinSoil, 
                                 colnames(LoevesteinSoil)[2:5],
                                 names_to = 'Period',
                                 values_to = 'conc')
  

  ### ===================== Add simulation parameters ================= ###
  
  # Birth days of the cattle ordered by ID 
  birth_day <- c( "2019-03-06", "2013-03-31", "2020-01-09", "2021-02-05", # Beuningen ID 15:18
                  "2021-02-05", "2022-01-21", "2021-12-17", # Beuningen ID 19:21
                  "2019-03-20", "2019-03-24", "2021-06-24", "2021-02-27", # Loevestein ID 22:25
                 "2015-03-16", "2020-10-06", "2021-02-27") # Loevestein ID 26:28
                
               
  t_stop <- c(rep("2023-11-07", 7), rep("2023-10-31", 7))
  t_clean <- t_stop
  gender <- c(rep("cow",4),rep("bull",3),rep("cow",7))
  verificationData$id_idx <- as.numeric(verificationData$ID) - min(as.numeric(verificationData$ID)) + 1
    
  # Add cattle birth days
  verificationData <- verificationData %>% mutate(birthDay = birth_day[id_idx],
                                           TSTOP = t_stop[id_idx],
                                           tClean = t_clean[id_idx],
                                           gender = gender[id_idx])
  
  # Add time to measurements from start of birth year (= start of simulation)
  verificationData <- verificationData %>% mutate(time = birthDay %>% 
                                                  sapply(strsplit, split="-") %>% 
                                                  sapply("[[", 1) %>% 
                                                  sapply(paste0, "-01-01") %>%
                                                  difftime(Date) %>%
                                                  as.integer() %>%
                                                  abs())
  
  # Add time to stop simulation from start of birth year (= start of simulation)
  verificationData <- verificationData %>% mutate(TSTOP = birthDay %>% 
                                                  sapply(strsplit, split="-") %>% 
                                                  sapply("[[", 1) %>% 
                                                  sapply(paste0, "-01-01") %>%
                                                  difftime(t_stop[id_idx]) %>%
                                                  as.integer() %>%
                                                  abs())
  
  # Add time to stop contaminated feed from start of birth year (= start of simulation)
  verificationData <- verificationData %>% mutate(tClean = birthDay %>%
                                                  sapply(strsplit, split="-") %>% 
                                                  sapply("[[", 1) %>% 
                                                  sapply(paste0, "-01-01") %>%
                                                  difftime(t_clean[id_idx]) %>%
                                                  as.integer() %>%
                                                  abs())
  
  # Add flood timing from start of birth year (=start of simulation)
  verificationData <- verificationData %>% 
    mutate(tFlood = birthDay %>%
             sapply(strsplit, split="-") %>% 
             sapply("[[", 1) %>% 
             sapply(paste0, "-01-01") %>%
             difftime("2021-02-01") %>%
             as.integer(),
           dFlood = 28) %>%
    mutate(simFlood = ifelse(tFlood < 0, TRUE, FALSE),
           tFlood = abs(tFlood))
    
  
  replaceLOQ <- function(x) {
    for (i in 1:length(x)) {
      if (grepl("<",x[i])) {
        x[i] <- as.numeric(gsub("<", "", x[i])) / 2
      }
    }
    return(x)
  }
  
  BeuningenGrass[,"conc"] <- lapply(BeuningenGrass[,"conc"], replaceLOQ)
  BeuningenSoil[,"conc"] <- lapply(BeuningenSoil[,"conc"], replaceLOQ)
  LoevesteinSoil[,"conc"] <- lapply(LoevesteinSoil[,"conc"], replaceLOQ)
  
  grassSummary <- ddply(BeuningenGrass, c("congener", "Period"), summarise, mean = mean(as.numeric(conc), na.rm=T))
  soilSummaryBeuningen <-  ddply(BeuningenSoil, c("congener", "Period"), summarise, mean = mean(as.numeric(conc), na.rm=T))
  soilSummaryLoevestein <-  ddply(LoevesteinSoil, c("congener", "Period"), summarise, mean = mean(as.numeric(conc), na.rm=T))
  
  verificationData <- verificationData %>% mutate(cGrassMax = join(data.frame(congener = congener),
                                                                 grassSummary[grassSummary$Period == "Winter", ])$mean/0.88,
                                                cGrassMin = join(data.frame(congener = congener),
                                                                 grassSummary[grassSummary$Period == "Summer", ])$mean/0.88,
                                                cSoil = case_when(id_idx %in% c("1","2", "3", "4", "5", "6", "7") ~ join(data.frame(congener = congener),
                                                                                                                     soilSummaryLoevestein[soilSummaryLoevestein$Period == "Soil", ])$mean/0.88,
                                                                  id_idx %in% c("8","9", "10", "11", "12", "13", "14") ~ join(data.frame(congener = congener),
                                                                                                                          soilSummaryBeuningen[soilSummaryBeuningen$Period == "Soil", ])$mean/0.88))
                                                  

  cols <- c("obs_cMeatFat.aSlow", "cGrassMax", "cGrassMin", "cSoil")
  tmpData <- verificationData[,c("obs_cMeatFat.aSlow")]
  verificationData[cols] <- lapply(verificationData[,cols], replaceLOQ)
  verificationData[cols] <- lapply(verificationData[,cols], as.numeric)
  
  # Round given time points for CINT
  tcols <- c("TSTOP", "tClean", "time", "tFlood")
  verificationData[tcols] <- lapply(verificationData[tcols], round_any, CINT) 
  
  getTEQ <- function(x,tef,year="2005") {
    names(tef)[1] <- "congener"
    if(year=="2005") names(tef)[2]<-"tef"
    else names(tef)[3]<-"tef"
    names(x)[2]<-"obs"
    x <- x %>% merge(tef) %>% mutate(teq = obs*tef)
    return(sum(x$teq, na.rm=T))
  }
  
  # Calculate TEQ2005 and TEQ2022
  TEQ <- data.frame()
  for (id in unique(verificationData$ID)) {
    dat <- verificationData[verificationData$ID==id,]
    
    dates <- unique(dat$Date)
    for (date in 1:length(dates)) {
      
      TEQ2005 <- data.frame(congener='TEQ2005',
                            ID=id,
                            obs_cMeatFat.aSlow=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cMeatFat.aSlow")], tef, year="2005"),
                            Date=dates[date],
                            id_idx=dat$id_idx[date],
                            birthDay=dat$birthDay[date],
                            TSTOP=dat$TSTOP[date],
                            tClean=dat$tClean[date],
                            gender=dat$gender[date],
                            time=dat$time[date],
                            tFlood=dat$tFlood[date],
                            dFlood=dat$dFlood[date],
                            simFlood=dat$simFlood[date],
                            cGrassMax=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMax")], tef, year="2005"),
                            cGrassMin=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMin")], tef, year="2005"),
                            cSoil=getTEQ(dat[dat$Date==dates[date],c("congener", "cSoil")], tef, year="2005"))
      TEQ2022 <- data.frame(congener='TEQ2022',
                            ID=id,
                            obs_cMeatFat.aSlow=getTEQ(dat[dat$Date==dates[date],c("congener", "obs_cMeatFat.aSlow")], tef, year="2022"),
                            Date=dates[date],
                            id_idx=dat[dat$Date==dates[date],'id_idx'],
                            birthDay=dat$birthDay[date],
                            TSTOP=dat$TSTOP[date],
                            tClean=dat$tClean[date],
                            gender=dat$gender[date],
                            time=dat$time[date],
                            tFlood=dat$tFlood[date],
                            dFlood=dat$dFlood[date],
                            simFlood=dat$simFlood[date],
                            cGrassMax=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMax")], tef, year="2022"),
                            cGrassMin=getTEQ(dat[dat$Date==dates[date],c("congener", "cGrassMin")], tef, year="2022"),
                            cSoil=getTEQ(dat[dat$Date==dates[date],c("congener", "cSoil")], tef, year="2022"))
      
      TEQ <- rbind(TEQ, rbind(TEQ2005,TEQ2022))
      
    }
  } 
  verificationData[,c("obs_cMeatFat.aSlow")] <- tmpData
  verificationData <- rbind(verificationData, TEQ)
  return(verificationData)
}
  
verificationData <- f.preprocess_verification()
