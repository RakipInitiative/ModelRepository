

#################################
#################################
#################################
# DRM
#################################
#################################
#################################

dose <- matrix(10^(seq(0,9,by=0.1)),91,10000)
Pprev <- 0.25
alphaGamma <- 0.145
betaGamma <- 7.59
condPillinf <- 0.33
Cretlog <- seq(-2,7,by=0.1)


#################################
#model
#################################
# the abscissa Cretlog MUST have the same length as the number of rows of the dose-matrix!


modDoseResponseMedianChallenge <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
  
  
  nrOfCrets <- dim(dose)[1]
  nrOfDoses <- dim(dose)[2]
  
  condPillinf <- condPillinf*matrix(1,nrOfCrets,nrOfDoses)
  
  Pill <- matrix(NA, nrOfCrets, nrOfDoses)
  
  for (i in 1:nrOfDoses) {
    Pill[1:nrOfCrets,i] <- condPillinf[1:nrOfCrets,i]*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[1:nrOfCrets,i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[1:nrOfCrets,i]))),4)
  }
  Pillmean <- rowMeans(Pill)
  Qill <- Pillmean*Pprev
  PrevExp <- Pprev*(sum(dose>0)/nrOfDoses)
  
  
  
  return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
}

runDoseResponseMedianChallenge <- modDoseResponseMedianChallenge(dose, 
                                                                 Pprev, 
                                                                 condPillinf, 
                                                                 alphaGamma, betaGamma)

Qill <-runDoseResponseMedianChallenge$Qill
PrevExp <- runDoseResponseMedianChallenge$PrevExp
Pillmean <-runDoseResponseMedianChallenge$Pillmean
Pill <-runDoseResponseMedianChallenge$Pill

#################################
#################################
#visualisation
#################################
plot(Cretlog, Pillmean, 
     xlab = "log(C_ret)", 
     ylab = "P_ill", 
     main = "probability of illness as a function of doses")
#################################
