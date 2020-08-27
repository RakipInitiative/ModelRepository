

#################################
#################################
#################################
# DRM
#################################
#################################
#################################

logDoses <- seq(0,8,by=0.1)
Pprev <- 0.25
alphaGamma <- 0.145
betaGamma <- 7.59
condPillinf <- 0.33


#################################
#model
#################################
# the abscissa Cretlog MUST have the same length as the number of rows of the dose-matrix!
doses <- 10^logDoses

modDoseResponseClassical <- function(doses, 
                                     condPillinf, 
                                     alphaGamma, betaGamma){
  
  Pillmean <- condPillinf*round((1 - exp(lgamma(alphaGamma+betaGamma)
                                     +lgamma(betaGamma+doses)
                                     -lgamma(betaGamma)
                                     -lgamma(alphaGamma+betaGamma+doses))),4)
  return(list(Pillmean=Pillmean))
}

runDoseResponseClassical <- modDoseResponseClassical(doses,
                                                     condPillinf, 
                                                     alphaGamma, betaGamma)

Pillmean <-runDoseResponseClassical$Pillmean

#################################
#################################
#visualisation
#################################
plot(logDoses, Pillmean, 
     xlab = "log(doses) [CFU]", 
     ylab = "P_ill", 
     main = "probability of illness as a function of doses")
#################################
