# INITIAL

gender_specifications <- function(parameters) {
  #' Set gender specific parameters
  #' 
  #' @param parameters list of parameters
  #' 
  #' The parameters for milk production, the milk-fat partition coefficient and
  #' and the time when the individual starts lactating are set. An updated
  #' parameter list is returned.
  with(parameters, {
    
    # Gender-specific parameters
    if (gender == "bull") {
      milkProd <- 0 # Milk production [L/day];
      meatFatPerc <- 2.0 # percentage of fat in meat [%]; user should be able to vary this;
      
      rVBlood <- rVBloodBull # Blood volume [-];
      rVFat <- rVFatBull #35 # relative fat volume [-];
      rVLiver <- rVLiverBull # relative liver volume [-];
      rVRich <- rVRichBull # relative richly perfused organs volume [-];
      rVSlow <- rVSlowBull # relative slowly perfused organs volume [-];
      
      
      
    } else if (gender == "cow") {
      milkProd <- 7.5 # Milk production [L/day];
      meatFatPerc <- 2.0 # percentage of fat in meat [%]; user should be able to vary this;
      
      rVBlood <- rVBloodCow # Blood volume [-];
      rVFat <- rVFatCow # relative fat volume [-];
      rVLiver <- rVLiverCow # relative liver volume [-];
      rVRich <- rVRichCow # relative richly perfused organs volume [-];
      rVSlow <- rVSlowCow # relative slowly perfused organs volume [-];
    }
    
    # return all variables in this function's environment
    as.list(sys.frame(sys.nframe()))
  })
}

calculate_parameters <- function(parameters) {
  #' Set birthday and flood timing as number of days since simulation start
  #' 
  #' @param parameters list of parameters
  #' 
  #' An updated
  #' parameter list is returned.  
  with(parameters, {
    # Define birthday of calf
    if (is.numeric(birthDay)) {
      tBirth <- birthDay
    } else {
      startYear <- strsplit(birthDay, "-")[[1]][1]
      tBirth <- as.integer(round(difftime(birthDay, paste0(startYear, "-01-01"))))
    }
    
    if (is.numeric(tFlood)) {
      tFlood <- tFlood
    } else {
      startYear <- strsplit(birthDay, "-")[[1]][1]
      tFlood <- as.integer(round(difftime(tFlood, paste0(startYear, "-01-01"))))
    }
    
    # return all variables in this function's environment
    as.list(sys.frame(sys.nframe()))
  }) # end with
}

assign_parameters <- function() {
  #' Assign default parameters and return them as a named list
  
  # Set default gender to bull. This will affect milkProd, pMilkFat, fatPerc, 
  # tLact, and bwLifeMean
  gender <- "bull"
  
  # Timing units
  unit <- "days"

  # timing commands
  TSTOP <- 15 * 365 # input$tstop # length of experiment [days]
  CINT <- 1 # Communication interval
  tSum <- 90 # Start of summer [days]
  tWin <- 275 # Start of winter [days]
  tYear <- 365 # number of days in a year [days]
  birthDay <- 0 # Day of the year at which the cow was born [day] or ["yyyy-mm-dd"];

  # PBK model parameters
  WGI <- 0.25 # fraction of GI-tract content [-]
  bwRef <- 450 # Reference weight [kg];
  #fatVariation <- 26 # Amplitude of seasonal variation in fat compartment [kg];
  relFatVariation <- 0.0 #37 # corresponds with 26 kg for cattle of 700 kg.
  q0Milk <- 86500 # cardiac output during milk production [L/day];
  q0Dry <- 72600 # cardiac  output when no milk production [L/day];
  rQFat <- 0.038 # relative blood flow fat compartment [-];
  rQLiver <- 0.458 # relative blood flow liver compartment [-];
  rQRich <- 0.304 # relative blood flow richly perfused organs [-];
  rQSlow <- 0.200 # relative blood flow slowly perfused organs [-];
  fQ <- 3 # diffusion limiting flow factor [-];
  pFat <- 280 # Fat partition coefficient [-];
  pLiver <- 23 # Liver partition coefficient [-];
  pRich <- 4 # Richly perfused organ partition coefficient [-];
  pSlow <- 8 # Slowly perfused organs partition coefficient [-];
  bloodFatPerc <- 0.21 # percentage of fat in blood plasma [%];
  pMilkFat <- 460 # Milk-fat partition coefficient [-];
  fatPerc <- 4.4 # Fat percentage in milk [%];
  tLact <- 2*365; # Number of days it takes until cows start lactating [days]. 
  kMet <- 14 # Metabolic rate constant [day-1]
  
  # Compartment volumes
  rVBloodBull <- 0.093 # Blood volume [-];
  rVFatBull <- 0.072 #135 # relative fat volume [-];
  rVLiverBull <- 0.019 # relative liver volume [-];
  rVRichBull <- 0.069 # relative richly perfused organs volume [-];
  rVSlowBull <- 1 - rVBloodBull - rVFatBull - rVLiverBull - rVRichBull
  rVBloodCow <- 0.093 # Blood volume [-];
  rVFatCow <- 0.135 # relative fat volume [-];
  rVLiverCow <- 0.019 # relative liver volume [-];
  rVRichCow <- 0.069 # relative richly perfused organs volume [-];
  rVSlowCow <- 1 - rVBloodCow - rVFatCow - rVLiverCow - rVRichCow # relative slowly perfused organs volume [-];

  # Exposure parameters in grass, soil and (mother) milk
  iGrassFraction <- 0.02 # Average grass intake (>6 months) per kg bw per day [kg/kg bw/ day]; User should be able to vary this
  #fGrass <- 0.25 # Fraction of dioxin absorbed from grass [-];
  tHGrass <- 30 # half time decay of dioxin concentration in grass [day];
  kGrass <- log(2) / tHGrass # exponential growth factor [per day];
  
  #fSoil <- 0.43 # fraction of dioxin in soil absorbed [-]
  iSoilFractionSummer <- 0.04 # soil intake in summer (as a fraction of grass intake) [-]; User should be able to vary this
  iSoilFractionWinter <- 0.08 # soil intake in winter (between 1 dec and 1 april) (as a fraction of grass intake) [-]; User should be able to vary this

  #fMilk <- 1.00  # fraction of dioxin in milk absorbed [-] Worst-case assumption
  iMilk <- 0.1 # Average milk intake as an calf (<6 months) per kg bw [kg/kg bw]; User should be able to vary this (default: https://futurebeef.com.au/resources/calf-rearing/)
  cMilk <- 0 # Concentration of dioxins in milk (ng/kg dry matter)
  
  fAbs <- 0.3 # Average absorption of substance through grass/soil/milk
  
  # Concentrations in standard scenario
  cGrassMax <- 9.7 # Maximum dioxin concentration in grass [ng/kg dry matter]; User should be able to vary  this
  cGrassMin <- 0.3 # Minimum dioxin concentration in grass [ng/kg dry matter]; User should be able to vary  this
  cSoil <- 16.2 # contamination of dioxin in soil [ng/kg]; User should be able to vary  this

  # Concentrations in after flood
  simFlood <- FALSE # Whether a flood is simulated [boolean]
  tFlood <- 3*365+30 # Time after start of simulation that flood takes place [day]
  dFlood <- 21 # Duration of the elevated soil intake after a flood [day]
  iSoilFractionFlood <- 0.66 # soil intake fraction after flood [-]

  # Concentration after moving to clean areas
  simClean <- FALSE # Whether a flood is simulated [boolean]
  tClean <- 365 * 10 + 90 # input$tClean
  cSoilClean <- 1.54 # contamination of dioxin in soil after moving to cleaner grounds [ng/kg]; User should be able to vary  this
  cGrassClean <- 0.492 # Reference dioxin concentration in grass [ng/kg dry matter]. This is used to simulate when moving cattle to cleaner grounds;

  tStartGrassIntake <- 180 # Age at which grass (and soil) intake start, and thus milk intake stops
  
  # return all variables in this function's environment
  as.list(sys.frame(sys.nframe()))
}

convert_time_scale <- function(parameters, current_unit="days", new_unit="weeks") {
  #' Change the time scale of all time-related parameters in the list. 
  #' 
  #' @param parameters List of parameters
  #' @param current_unit Current time unit of the parameters. 
  #' Either "days", "weeks" or "months". Here, a month consists of 28 days
  #' @param new_unit New time unit of the parameters. 
  #' Either "days", "weeks" or "months". Here, a month consists of 28 days
  
  if (current_unit== "days" & new_unit == "days") factor <- 1
  if (current_unit== "days" & new_unit == "weeks") factor <- 7
  if (current_unit== "days" & new_unit == "months") factor <- 28
  if (current_unit== "weeks" & new_unit == "days") factor <- 1/7
  if (current_unit== "weeks" & new_unit == "weeks") factor <- 1
  if (current_unit== "weeks" & new_unit == "months") factor <- 28/7
  if (current_unit== "months" & new_unit == "days") factor <- 1/28
  if (current_unit== "months" & new_unit == "weeks") factor <- 7/28
  if (current_unit== "months" & new_unit == "months") factor <- 1
  if (is.null(factor)) warning("provide time scaling units as either 'days', 'weeks' or 'months'")
  
  with(parameters, {
    
    TSTOP <- ceiling(TSTOP / factor)
    tSum <- round(tSum / factor )
    tWin <- round(tWin /factor )
    tYear <- round(tYear / factor)
    tBirth <- round(tBirth / factor )
    
    q0Milk <- q0Milk * factor
    q0Dry <- q0Dry * factor
    
    tLact <- round(tLact / factor)
    kMet <- kMet * factor
  
    iGrassFraction <- iGrassFraction * factor
    tHGrass <- tHGrass / factor
    kGrass <- kGrass * factor
    iMilk <- iMilk * factor
    milkProd <- milkProd * factor
    tFlood <- round(tFlood / factor)
    dFlood <- round(dFlood / factor)
    tClean <- round(tClean / factor)
    tStartGrassIntake <- round(tStartGrassIntake / factor)
    
    # return all variables in this function's environment
    as.list(sys.frame(sys.nframe()))
    })
}
  

get_intake_fraction <- function(bwLifeMean){
  #' Compute scaling fraction for intake
  #' 
  #' @param bwLifeMean body weight, not taking seasonal variation into account.
  bwAdult <- 700

  # Intake fraction. Intake of the cattle is linearly scaled with respect to the intake (15kg) of cattle weighing 700 kg. # TODO: Check if correct and realistic
  # TODO: Check if correct and realistic
  bwLifeMean / bwAdult
}
  
  
calculate_dose <- function(t, bwLifeMean, parameters) {
  #' Compute dose for a given time
  #' 
  #' @param t current time. Unit of time is equal to the unit parameter
  #' @param bwLifeMean body weight at time t, not taking seasonal variation into account.
  #' @param parameters list of parameters
  #' 
  #' Compute dioxin intake based on weight and season. 
  
  with(parameters, {
    
    # Create array of dose timing for each year
    doseTiming <- t %% tYear
    
    # No dose before birth
    if (t < tBirth) {
      dose <- 0
    } 
    
    else if (t<(tBirth+tStartGrassIntake)) { 
      
      inMilk <- iMilk * bwLifeMean # intake of calves scaled to current body weight [kg]
      dose <- iMilk * bwLifeMean * cMilk * fAbs
      
    }
    
    else {
      #intakeFraction <- get_intake_fraction(bwLifeMean)

      # time dependent intake parameters
      iGrass <- iGrassFraction * bwLifeMean # intake of cattle scaled to current body weight [kg]
      
      # Calculate dioxin intake per day through grass in the specified scenario
      if (doseTiming < tSum) {
        cGrass <- pmin(cGrassMin * exp(kGrass * (doseTiming + (tYear - tWin))), cGrassMax)
      } else if (doseTiming >= tSum & doseTiming < tWin) {
        cGrass <- pmax(cGrassMax * exp(-kGrass * (doseTiming - tSum)), cGrassMin)[doseTiming >= tSum & doseTiming < tWin]
      } else if (doseTiming >= tWin) {
        cGrass <- pmin(cGrassMin * exp(kGrass * (doseTiming - tWin)), cGrassMax)[doseTiming >= tWin]
      }

      if (doseTiming >= tSum & doseTiming < tYear * 11/12) { # From november till 1st of april, elevated soil intake is assumed
        iSoil <- iSoilFractionSummer * iGrass # soil intake summer [kg/day]
      } else  {
        iSoil <- iSoilFractionWinter * iGrass # soil intake winter [kg/day]
      }
      
      
      # Simulate elevated grass levels after flood
      if (simFlood == TRUE) {
        if (t > tFlood & t < (tFlood + dFlood)) {
          iSoil <- iGrass * iSoilFractionFlood
        }
      }

      # After tClean, cGrass becomes cGrassRef
      if (simClean == TRUE) {
        if ((t - tBirth) >= tClean) {
          cGrass <- cGrassClean
          cSoil <- cSoilClean
        }
      }

      inGrass <- fAbs * iGrass * cGrass
      inSoil <- fAbs * iSoil * cSoil

      dose <- inGrass + inSoil
    }
    # return all variables in this function's environment
    return(dose)
  }) # end with
}

get_bwLifeMean <- function(t, tBirth, gender, unit){
  #' Compute mean body weight at current time.
  #'
  #' The mean body weight is based on the age and gender of the individual.
  #' @param t current time in days
  #' @param tBirth day on which individual was born
  #' @param gender individual's gender
  #' 
  #' The mean body weight represents the body weight at a given moment, not 
  #' taking into consideration the seasonal variations. Bulls and cows grow
  #' exponentially until a predefined equilibrium weight is reached.
  #' When a time earlier than birth in supplied, 1 will be returned to
  #' prevent division by zero.
  
  if (unit=="days") factor <- 1
  if (unit=="weeks") factor <- 7
  if (unit=="months") factor <- 28

  if (t < tBirth) {
    # placeholder body weight to avoid division by zero before calf is born
    bwLifeMean <- 1
  } else {
    switch(gender,
           # This formula was derived with units of days. If other time units are used, they need to be scaled back to days, hence the multiplication with factor
           "bull" = {
             bwLifeMean <- 1669 - 1627 * exp(-0.2237 * (t - tBirth) *factor / 365) 
           },
           "cow" = {
             bwLifeMean <- 1028 - 944 * exp(-0.2593 * (t - tBirth)  *factor / 365)
           }
    )
  }  
  return(bwLifeMean)
}

get_w_non_GI <- function(bwLifeMean, WGI){
  #' Compute the weight excluding the GI tract.
  #'
  #' @param bwLifeMean body weight, not taking seasonal variation into account.
  #' @param WGI fractional GI tract weight
  bwLifeMean * (1 - WGI)
}

get_bw_deviation <- function(t, tYear, relFatVariation, bwLifeMean){
  
  #' Compute the body weight deviation due to seasonal variation.
  #' 
  #' @param t current time [unit defined by user]
  #' @param tYear number of time units in a year 
  #' @param relFatVariation relative fat fraction
  #' @param bwLifeMean body weight, not taking seasonal variation into account.
  relFatVariation * bwLifeMean * sin(2 * pi * (t) / tYear)
  
  }

get_kinetic_pars <- function(t, bw, clM, parameters){
  #' Compute the value of kinetic parameters at time t
  #' 
  #' @param t current time in days
  #' @param bw body weight
  #' @param parameters names list of parameters
  with(parameters, {
    if (((t - tBirth) > tLact) & (t %% tYear < tWin) & (t %% tYear >= tSum)) {
      qC <- q0Milk * ((bw / bwRef)^0.75)
      clB <- pMilkFat * clM
      milkOn <- 1
    } else {
      qC <- q0Dry * ((bw / bwRef)^0.75)
      clB <- 0
      milkOn <- 0
    }
    return(list(qC=qC, clB=clB, milkOn=milkOn))
  })
}

derivative <- function(t, y, parameters, ...) {
  with(parameters, {
    
    bwLifeMean <- get_bwLifeMean(t, tBirth, gender, unit)
    intake <- calculate_dose(t, bwLifeMean, parameters)
    
    # time dependent weight parameters
    #bwFraction <- get_intake_fraction(bwLifeMean)
    
    # Calculate compartment volumes
    w_non_GI <- get_w_non_GI(bwLifeMean, WGI)
    vBlood <- w_non_GI * rVBlood
    vLiver <- w_non_GI * rVLiver
    vRich <- w_non_GI * rVRich
    vSlow <- w_non_GI * rVSlow
    vFat <- rVFat * w_non_GI - get_bw_deviation(t, tYear, relFatVariation, bwLifeMean)
    bw <- vFat + vBlood + vLiver + vRich + vSlow
    clM <- milkProd * fatPerc / 100
    
    # Define model compartments
    D <- y[1] # dose per day
    DoseMassBal <- y[2]
    aBlood <- y[3] # blood compartment
    aFat <- y[4] # fat compartment
    aRich <- y[5] # richly perfused organ compartment
    aSlow <- y[6] # slowly perfused organ compartment
    aLiver <- y[7] # liver compartment;
    aMet <- y[8] # metabolised particles;
    aMilk <- y[9] # particles cleared through milk

    # Calculate concentration in compartments
    cBlood <- aBlood / vBlood
    cFat <- aFat / vFat
    cLiver <- aLiver / vLiver
    cRich <- aRich / vRich
    cSlow <- aSlow / vSlow
 
    # update kinetic parameters
    list2env(get_kinetic_pars(t, bw, clM, parameters), .GlobalEnv)

    qF <- rQFat * qC
    qffq <- qF / fQ
    qR <- rQRich * qC
    qS <- rQSlow * qC
    qL <- rQLiver * qC
    clL <- vLiver * kMet

    # Define differential equations
    dD <- -intake
    dDoseMassBal <- intake # recording of the dose for the mass balance calculation
    daBlood <- -qffq * (cBlood - cFat / pFat) - qS * (cBlood - cSlow / pSlow) - qR * (cBlood - cRich / pRich) - qL * (cBlood - cLiver / pLiver) - clB * cBlood
    daFat <- qffq * (cBlood - cFat / pFat)
    daRich <- qR * (cBlood - cRich / pRich)
    daSlow <- qS * (cBlood - cSlow / pSlow)
    daLiver <- intake + qL * (cBlood - cLiver / pLiver) - clL * cLiver / pLiver
    daMet <- clL * cLiver / pLiver
    daMilk <- clB * cBlood

    # Mass Balance
    Total <- DoseMassBal
    Calculated <- D + aBlood + aFat + aSlow + aRich + aLiver + aMet + aMilk
    ERROR <- ((Total - Calculated) / Total + 1E-30) * 100
    MASSBBAL <- Total - Calculated + 1

    list(
      c(
        dD, dDoseMassBal, daBlood, daFat,
        daRich, daSlow,
        daLiver, daMet, daMilk
      ),
      c(
        ERROR = ERROR,
        MASSBBAL = MASSBBAL,
        cBlood = cBlood,
        cFat = cFat,
        cLiver = cLiver,
        cMilk = clB * cBlood / milkProd,
        cMeatFat = cSlow / (pSlow * 0.8 / pFat ),  #: pg/g fat
        cMeat = cSlow, #: pg/g meat
        cBloodFat = cBlood / (bloodFatPerc / 100),
        bw = bw,
        vBlood = vBlood,
        vLiver = vLiver,
        vFat = vFat,
        vSlow = vSlow,
        vRich = vRich,
        wgi = bwLifeMean * WGI,
        intake = intake
      )
    )
  })
}

run_model <- function(parameters) {
  
  parameters <- calculate_parameters(parameters)
  parameters <- gender_specifications(parameters)
  parameters <- convert_time_scale(parameters, current_unit="days", new_unit=parameters$unit)
  
  with(parameters, {
    
    TSTART <- 0.0
    times <- seq.int(TSTART, TSTOP, CINT)

    y <- c(
      D = 0,
      DoseMassBal = 0,
      aBlood = 0,
      aFat = 0,
      aRich = 0,
      aSlow = 0,
      aLiver = 0,
      aMet = 0,
      aMilk = 0
    )

    solution <- deSolve::ode(
      y,
      times,
      derivative,
      parameters,
      method = "lsodes"
    )

    return(as.data.frame(unclass(solution)))
  })
}

