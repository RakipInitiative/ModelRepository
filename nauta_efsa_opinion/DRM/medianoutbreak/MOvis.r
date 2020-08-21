

#################################
#################################
#################################
# DRM
#################################
#################################
#################################

dose <- matrix(10^(seq(0,9,by=0.1)),91,10000)
Pprev <- 0.25
alphaGamma <- 0.38
betaGamma <- 0.51
rPillinf <- 0.76
etaPillinf <- 0.0092
Cretlog <- seq(-2,7,by=0.1)


#################################
#model
#################################
# the abscissa Cretlog MUST have the same length as the number of rows of the dose-matrix!

condPillinf <- 1 - (1+dose/etaPillinf)^(-rPillinf)
modDoseResponseMedianChallenge <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
  
  
  nrOfCrets <- dim(dose)[1]
  nrOfDoses <- dim(dose)[2]
  
  Pill <- matrix(NA, nrOfCrets, nrOfDoses)
  
  for (i in 1:nrOfDoses) Pill[1:nrOfCrets,i] <- condPillinf[1:nrOfCrets,i]*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[1:nrOfCrets,i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[1:nrOfCrets,i]))),4)
  
  Pillmean <- rowMeans(Pill)
  Qill <- Pillmean*Pprev
  PrevExp <- Pprev*(sum(dose>0)/nrOfDoses)
  
  
  
  return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
  #return(list(Qill=Qill, PrevExp=PrevExp))
}

runDoseResponseMedianChallenge <- modDoseResponseMedianChallenge(dose, Pprev, condPillinf, alphaGamma, betaGamma)
#runDoseResponseMedianChallenge <- modDoseResponseMedianChallenge(dose=runConsumerPhaseNauta$dose, Pprev, condPillinf, alphaGamma, betaGamma)

Qill <-runDoseResponseMedianChallenge$Qill
PrevExp <- runDoseResponseMedianChallenge$PrevExp
Pillmean <-runDoseResponseMedianChallenge$Pillmean
Pill <-runDoseResponseMedianChallenge$Pill

#################################
#################################
#visualisation
#################################
plot(Cretlog, Pillmean, xlab = "log(C_ret)", ylab = "P_ill", main = "probability of illness based on contamination of chicken meat bought at retail")
#################################
