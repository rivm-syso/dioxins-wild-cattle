#' Estimate growth curves of Rode Geus cows and bulls based on data provided by 
#' WFSR

library(ggplot2)

# Bulls
bull <- data.frame(weight = c(140, 200, 300, 450, 720, 800, 1000, 1250),
                   age    = c(0.25, 0.5, 0.75, 1.5, 2, 3, 4, 6))

brodyModel <- nls(weight ~ b1 + b2*exp(b3*age), data=bull, start=c(b1=700, b2=700, b3=-0.002*365))
print(brodyModel)
tt<-seq(0,15,1/365)
y <- 1669 - 1627*exp(-0.2237*tt)

print(ggplot() + 
  geom_line(aes(x=tt, y=y), size=2) + 
  geom_point(aes(x=bull$age, y=bull$weight), color='red', size=4) + 
  ggtitle("Rode Geus bull body weight") +
  xlab("Age [year]") + 
  ylab("Average body weight [kg]"))


# Cows
cow <- data.frame(weight = c(140, 200, 240, 400, 540, 430, 620, 720, 825),
                  age    = c(0.25, 0.5, 0.75, 1.5, 2, 2.5, 3, 5, 5))

brodyModel <- nls(weight ~ b1 + b2*exp(b3*age), data=cow, start=c(b1=700, b2=700, b3=-0.002*365))
print(brodyModel)
tt<-seq(0,15,1/365)
y <- 1028 - 944*exp(-0.2593*tt)

print(ggplot() + 
  geom_line(aes(x=tt, y=y), size=2) + 
  geom_point(aes(x=cow$age, y=cow$weight), color='red', size=4) + 
  ggtitle("Rode Geus cow body weight") +
  xlab("Age [year]") + 
  ylab("Average body weight [kg]"))

