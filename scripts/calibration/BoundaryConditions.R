# 1,2,3,4,6,7,8-HpCDD
parm.names <- c("rVFatBull", "rVFatCow", "pFat","pLiver", "pRich","pSlow","kMet")
lowerBound <- c(0.04, 0.06, 100,  1,  1,  1,  10, 0,0)
upperBound <- c(0.12, 0.14, 400, 500,10, 20, 80,0.8,0.8)

# 2,3,7,8-TCDD
parm.names <- c("rVFatBull", "rVFatCow", "pFat","pLiver", "pRich","pSlow","kMet", "fGrass", "fSoil")
lowerBound <- c(0.04, 0.04, 200,  1,  1,  1,  1, 0.2,0.2)
upperBound <- c(0.12, 0.12, 400, 50,10, 20, 30, 0.8,0.8)