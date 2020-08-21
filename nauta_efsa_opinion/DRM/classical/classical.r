#parameters
dose <- rep(1,100000)
alphaGamma <- 0.145
betaGamma <- 7.59
Pprev <- 0.25
condPillinf <- 0.33



#model
modDoseResponseClassical <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
  
  niter <- length(dose)
  
  Pill <- array(NA, niter)
  
  for (i in 1:niter) Pill[i] <- condPillinf*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[i]))),4)
  
  Pillmean <- mean(Pill)
  Qill <- Pillmean*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  
  #return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
  return(list(Qill=Qill, PrevExp=PrevExp))
}

runDoseResponseClassical <- modDoseResponseClassical(dose, Pprev, condPillinf, alphaGamma, betaGamma)
#runDoseResponseClassical <- modDoseResponseClassical(dose=runConsumerPhaseNauta$dose, Pprev, condPillinf, alphaGamma, betaGamma)

Qill <-runDoseResponseClassical$Qill
PrevExp <- runDoseResponseClassical$PrevExp


#visualisation
library(gridExtra)

results <- cbind(round(runDoseResponseClassical$PrevExp*100,1),
                 round(runDoseResponseClassical$Qill*100,2))
rownames(results) <- c("Classical DR")
colnames(results) <- c('Prevalence of Exposure [%]', 'Q_ill [%]')
grid.table(results)