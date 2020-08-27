

#################################
#################################
#################################
# DRM
#################################
#################################
#################################

logDoses <- seq(0,8,by=0.1)
alphaGamma <- 0.44
betaGamma <- 0.51
rPillinf <- 0.06
etaPillinf <- 0.88


#################################
#model
#################################
# the abscissa Cretlog MUST have the same length as the number of rows of the dose-matrix!
doses <- 10^logDoses
condPillinf <- 1 - (1+doses/etaPillinf)^(-rPillinf)
modDoseResponseMedianChallenge <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
  
  Pillmean <- condPillinf*round((1 - exp(lgamma(alphaGamma+betaGamma)
                                         +lgamma(betaGamma+doses)
                                         -lgamma(betaGamma)
                                         -lgamma(alphaGamma+betaGamma+doses))),4)
  return(list(Pillmean=Pillmean))
}

runDoseResponseMedianChallenge <- modDoseResponseMedianChallenge(dose, Pprev, condPillinf, alphaGamma, betaGamma)

Pillmean <-runDoseResponseMedianChallenge$Pillmean

#################################
#################################
#visualisation
#################################
plot(logDoses, Pillmean, 
     xlab = "log(doses) [CFU]", 
     ylab = "P_ill", 
     main = "probability of illness as a function of doses")
#################################
