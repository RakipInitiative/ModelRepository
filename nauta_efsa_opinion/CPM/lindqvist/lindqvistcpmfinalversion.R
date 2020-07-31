# set.seed(1234)
transferRateLower <- -3.7
transferRateUpper <- -1
meanPortion <- 189.0
sigmaCret <- 0.2
muCret <- 1.5
Pprev <- 0.25
niter <- 100000
upperPortion <- 1000.0
stdPortion <- 126.9


# model
require(distr)

modConsumerPhaseLindqvist <- function(niter, meanPortion, stdPortion, upperPortion, transferRateLower, transferRateUpper, muCret, sigmaCret, Pprev){
  
  

  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)
  
  # initialising of needed arrays -> TODO do i need all of them?
  Cretlog <- array(NA, niter)
  Cret <- array(NA, niter)
  Wc <- array(NA, niter)
  Ncarcass <- array(NA, niter)
  pTR <- array(NA, niter)
  dose <- array(NA, niter)
  
    # transfer rate BA4
  X <- runif(niter,transferRateLower,transferRateUpper)
  
  # AZ4
  NpDistribution <- DiscreteDistribution(supp=c(1,2,3,4,5),prob=c(0.28,0.48,0.11,0.09,0.04))
  Np <- r(NpDistribution)(niter)
  
  # parameter for gaussian mixture distribution -> Cretlog
  K <- length(muCret)
  
  for (i in 1:niter){
    Cretlog[i] <- rnorm(1, muCret, sigmaCret)
    Cret[i] <- 10^Cretlog[i]
    
    Ncarcass[i] <- rpois(1,1097*Cret[i])
    
    Wc[i] <- min(rlnorm(1, muWc, sigmaWc), upperPortion)
    
    pTR[i] <- min(1,Np[i]*Wc[i]/1097*10^X[i])
    
    dose[i] <- ifelse(Ncarcass[i]==0, 0, rbinom(1, size=Ncarcass[i], prob=pTR[i]))
  }
  
  
  dosemean <- mean(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  return(list(dose=dose, dosemean=dosemean, PrevExp = PrevExp)) 
}

runConsumerPhaseLindqvist <- modConsumerPhaseLindqvist(niter, meanPortion, stdPortion, upperPortion, transferRateLower, transferRateUpper, muCret, sigmaCret, Pprev)

dose <- runConsumerPhaseLindqvist$dose
dosemean <- runConsumerPhaseLindqvist$dosemean
PrevExp <- runConsumerPhaseLindqvist$PrevExp
																			

# visualization
library(gridExtra)

resultsCPM <- cbind(round(runConsumerPhaseLindqvist$dosemean,0), round(runConsumerPhaseLindqvist$PrevExp*100,1))
rownames(resultsCPM) <- c("CPM Lindqvist")
colnames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]')

tableCPM <- matrix(round(runConsumerPhaseLindqvist$dose,0))
rownames(tableCPM) <- rownames(tableCPM, do.NULL=FALSE, prefix="Value.")
colnames(tableCPM) <- c('Dose')
write.csv(tableCPM, file="doses-consumer-phase-Lindqvist.csv")

sortdose <-sort(log10(runConsumerPhaseLindqvist$dose))
vsize <-length(sortdose)
pdose <- array(NA,vsize)
for (i in 1:vsize)
{pdose[i] <- (i-0.5)/vsize}

plot(sortdose,pdose, 
     xlab = "log Dose", ylab = "",
     main = "doses from meat contaminated at retail")
grid.table(resultsCPM)																																																																																																													