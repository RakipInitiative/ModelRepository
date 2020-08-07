meanPortion <- 189.0
stdPortion <- 126.9
muCret <- 1.5
sigmaCret <- 0.2
Pprev <- 0.25
niter <- 100000
upperPortion <- 1000.0


abscissaMeat2Kitchenware <- c(0,0.002,0.004,0.009,0.011,0.012,0.014,0.016,0.019,0.023)
ordinateMeat2Kitchenware <- c(0,0.18,0.27,0.45,0.54,0.64,0.73,0.82,0.91,1)

abscissaKitchenware2Meat <- c(0,5,6.7,7.7,9.1,10.5,14.3,20,33.3)
ordinateKitchenware2Meat <- c(0.18,0.27,0.45,0.54,0.64,0.73,0.82,0.91,1)

abscissaMeat2Hands <- c(0,0.2,0.3,0.6,0.9,1,1.2,1.6,2.5,2.6,3.6,7.8)
ordinateMeat2Hands <- c(0,0.09,0.18,0.27,0.36,0.45,0.54,0.64,0.73,0.82,0.91,1)

abscissaHands2Meat <- c(0,0.4,0.8,1.8,1.9,9.5)

# from @Risk code
ordinateHands2Meat <- c(0,0.2,0.4,0.6,0.61,1)

#Marcel thinks this should be 
#ordinateHands2Meat <- c(0,0.2,0.4,0.6,0.8,1)

probHands <- 0.25917
probEnv <- 0.14208




library(mc2d)

modConsumerPhaseCalistri <- function(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, 
                                     abscissaMeat2Kitchenware, ordinateMeat2Kitchenware, 
                                     abscissaKitchenware2Meat, ordinateKitchenware2Meat, 
                                     abscissaMeat2Hands, ordinateMeat2Hands, 
                                     abscissaHands2Meat, ordinateHands2Meat, 
                                     probHands, probEnv){
  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)
  
  #sampling from approximated empirical distribution
  napprox <- 1000
  
  # empirical distribution for transfer probability from Meat to Kitchenware equipment
  abscissaMeat2Kitchenwareapprox <- approx(abscissaMeat2Kitchenware, ordinateMeat2Kitchenware, n=napprox)$x
  ordinateMeat2Kitchenwareapprox <- approx(abscissaMeat2Kitchenware, ordinateMeat2Kitchenware, n=napprox)$y
  PMeat2Kitchenwareapprox <- diff(c(0,ordinateMeat2Kitchenwareapprox))
  
  # empirical distribution for transfer probability from Kitchenware equipment to Meat
  abscissaKitchenware2Meatapprox <- approx(abscissaKitchenware2Meat, ordinateKitchenware2Meat, n=napprox)$x
  ordinateKitchenware2Meatapprox <- approx(abscissaKitchenware2Meat, ordinateKitchenware2Meat, n=napprox)$y
  PKitchenware2Meatapprox <- diff(c(0,ordinateKitchenware2Meatapprox))
  
  # empirical distribution for transfer probability from Meat to Hands equipment
  abscissaMeat2Handsapprox <- approx(abscissaMeat2Hands, ordinateMeat2Hands, n=napprox)$x
  ordinateMeat2Handsapprox <- approx(abscissaMeat2Hands, ordinateMeat2Hands, n=napprox)$y
  PMeat2Handsapprox <- diff(c(0,ordinateMeat2Handsapprox))
  
  # empirical distribution for transfer probability from Hands 2 Meat equipment
  abscissaHands2Meatapprox <- approx(abscissaHands2Meat, ordinateHands2Meat, n=napprox)$x
  ordinateHands2Meatapprox <- approx(abscissaHands2Meat, ordinateHands2Meat, n=napprox)$y
  PHands2Meatapprox <- diff(c(0,ordinateHands2Meatapprox))
  
  Cretlog <- array(NA, niter)
  Cret <- array(NA, niter)
  Wc <- array(NA, niter)
  Nportion <- array(NA, niter)
  Ptr <- array(NA, niter)
  dose <- array(NA, niter)
  
  for (i in 1:niter){
    Cretlog[i] <- rnorm(1, muCret, sigmaCret)
    Cret[i] <- 10^Cretlog[i]
    
    Wc[i] <- min(rlnorm(1, muWc, sigmaWc), upperPortion)
    
    Nportion[i] <- ifelse(Cret[i]*Wc[i]<1e3, rpois(1, Cret[i]*Wc[i]), round(rnorm(1, Cret[i]*Wc[i], sqrt(Cret[i]*Wc[i])),0))
    
    transferM2K <- sample(abscissaMeat2Kitchenwareapprox, 1, prob=PMeat2Kitchenwareapprox, replace=TRUE)
    transferK2M <- sample(abscissaKitchenware2Meatapprox, 1, prob=PKitchenware2Meatapprox, replace=TRUE)
    transferM2H <- sample(abscissaMeat2Handsapprox, 1, prob=PMeat2Handsapprox, replace=TRUE)
    transferH2M <- sample(abscissaHands2Meatapprox, 1, prob=PHands2Meatapprox, replace=TRUE)
    
    
    # AU34
    transferEnv <- transferM2K*transferK2M/100
    # AU36
    transferHands <- transferM2H*transferH2M/10000
    
    Ptr[i] <- transferEnv*rbinom(1,1,probEnv)+(1-transferEnv)*transferHands*rbinom(1,1,probHands)
    
    dose[i] <- ifelse(Nportion[i]==0, 0, rbinom(1, size=Nportion[i], prob=Ptr[i]))
  }
  
  dosemean <- mean(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  
  return(list(dose=dose, dosemean=dosemean, PrevExp = PrevExp))
}

runConsumerPhaseCalistri <- modConsumerPhaseCalistri(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, 
                                                     abscissaMeat2Kitchenware, ordinateMeat2Kitchenware, 
                                                     abscissaKitchenware2Meat, ordinateKitchenware2Meat, 
                                                     abscissaMeat2Hands, ordinateMeat2Hands, 
                                                     abscissaHands2Meat, ordinateHands2Meat, 
                                                     probHands, probEnv)

dose <- runConsumerPhaseCalistri$dose
dosemean <- runConsumerPhaseCalistri$dosemean
PrevExp <- runConsumerPhaseCalistri$PrevExp

# visualization
library(gridExtra)

resultsCPM <- cbind(round(dosemean,0), round(PrevExp*100,1))
rownames(resultsCPM) <- c("CPM Calistri")
colnames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]')

tableCPM <- matrix(round(dose,0))
rownames(tableCPM) <- rownames(tableCPM, do.NULL=FALSE, prefix="Value.")
colnames(tableCPM) <- c('Dose')
write.csv(tableCPM, file="doses-consumer-phase-Calistri.csv")

sortdose <-sort(log10(dose))
vsize <-length(sortdose)
pdose <- array(NA,vsize)
for (i in 1:vsize)
{pdose[i] <- (i-0.5)/vsize}

plot(sortdose,pdose, 
     xlab = "log Dose", ylab = "",
     main = "doses from meat contaminated at retail")
grid.table(resultsCPM)