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
                                values_to = 'obs_cMeatFat.aSlow') %>% mutate(Date = "2023-11-07")
    
  # Process data Beuningen
  Beuningen <- Data[c(1,15:21)]
  colnames(Beuningen)[1] <- 'congener'
  colnames(Beuningen)[2:8] <- c('15', '16', '17', '18', '19', '20', '21')

  BeuningenData <- pivot_longer(Beuningen, 
                                colnames(Beuningen)[2:8],
                                names_to = 'ID',
                                values_to = 'obs_cMeatFat.aSlow') %>% mutate(Date = "2023-11-07")
  
  verificationData <- rbind(BeuningenData, LoevesteinData)
  
  # Read intake data (assume Loevestein intake is the same as Beuningen)
  BeuningenIntake <- as.data.frame(read_xlsx("data/Beuningen1.xlsx", sheet='Grond en gras'))
  LoevesteinSoil <- as.data.frame(read_xlsx("data/Loevestein1.xlsx", sheet='Gras Grond Loev'))
  BeuningenIntake <- BeuningenIntake[BeuningenIntake$`Tabel 3A:` %in% congeners,c(1,5:13,15:21)] 
  LoevesteinSoil <- LoevesteinSoil[LoevesteinSoil$`Tabel 3A:` %in% congeners,c(1,8:11)] 
  colnames(BeuningenIntake)[1] <- 'congener'
  colnames(LoevesteinSoil)[1] <- 'congener'
  
  BeuningenGrass <- BeuningenIntake[, 1:10]
  BeuningenSoil <- BeuningenIntake[,c(1,11:17)]
  colnames(BeuningenGrass)[2:10] <- c('Winter', 'Winter', 'Control','Flood', 
                                       'Flood', 'Summer', 'Summer', 'Winter', 'Winter')
  colnames(BeuningenSoil)[2:8] <- c('Soil','Soil','Soil','Control','Soil','Soil','Soil')
  colnames(LoevesteinSoil)[2:5] <- c('Soil','Soil','Soil','Soil')
                                       
  BeuningenGrass <- pivot_longer(BeuningenGrass, 
                                colnames(BeuningenGrass)[2:10],
                                names_to = 'Period',
                                values_to = 'conc')
  BeuningenSoil <- pivot_longer(BeuningenSoil, 
                                 colnames(BeuningenSoil)[2:8],
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
                                                  

  
  # For every entry below LOQ, divide by 2.                                  
  cols <- c("obs_cMeatFat.aSlow", "cGrassMax", "cGrassMin", "cSoil")
  verificationData[cols] <- lapply(verificationData[,cols], replaceLOQ)
  verificationData[cols] <- lapply(verificationData[,cols], as.numeric)
  
  # Round given time points for CINT
  tcols <- c("TSTOP", "tClean", "time", "tFlood")
  verificationData[tcols] <- lapply(verificationData[tcols], round_any, CINT) 
  
  verificationData[verificationData$congener == 'WHO2005-PCDD/F-PCB-TEQ (ub)',]$congener <- 'Total-TEQ'
  
  return(verificationData)
}
  
verificationData <- f.preprocess_verification()
