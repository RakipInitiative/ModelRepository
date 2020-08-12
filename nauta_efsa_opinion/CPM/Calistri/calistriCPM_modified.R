# CPM
#################################
#parameters
#################################
niter <- 10000
Pprev <- 0.25
CretLogMin <- -2
CretLogMax <- 7
CretLogStep <- 0.1
muCret <- 1.5
sigmaCret <- 1.2
piCret <- 1.0
meanPortion <- 189.0
stdPortion <- 126.9
upperPortion <- 1000.0

abscissaMeat2Kitchenware <- c(0,0.002,0.004,0.009,0.011,0.012,0.014,0.016,0.019,0.023)
ordinateMeat2Kitchenware <- c(0,0.18,0.27,0.45,0.54,0.64,0.73,0.82,0.91,1)

abscissaKitchenware2Meat <- c(0,5,6.7,7.7,9.1,10.5,14.3,20,33.3)
ordinateKitchenware2Meat <- c(0.18,0.27,0.45,0.54,0.64,0.73,0.82,0.91,1)

abscissaMeat2Hands <- c(0,0.2,0.3,0.6,0.9,1,1.2,1.6,2.5,2.6,3.6,7.8)
ordinateMeat2Hands <- c(0,0.09,0.18,0.27,0.36,0.45,0.54,0.64,0.73,0.82,0.91,1)

abscissaHands2Meat <- c(0,0.4,0.8,1.8,1.9,9.5)

# from @Risk code
#ordinateHands2Meat <- c(0,0.2,0.4,0.6,0.61,1)
#Marcel thinks this should be 
ordinateHands2Meat <- c(0,0.2,0.4,0.6,0.8,1)

probHands <- 0.25917
probEnv <- 0.14208

maxDistsShown = 5


library(mc2d)

#############################


#################################
#model
#################################
# set.seed(1234) CretLogMin, CretLogMax, CretLogStep, 
myCPMname <- c("CPM Calistri")
modConsumerPhaseCalistri <- function(niter, 
                                     Pprev, 
                                     muCret, sigmaCret, piCret,
                                     CretLogMin, CretLogMax, CretLogStep, 
                                     meanPortion, stdPortion, upperPortion, 
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
  
  #random numbers from the mixture distribution
  rnorm.mixture <- function(n, K, mu.mix=rep(0,K), sigma.mix=rep(1,K), pi.mix=rep(0.5,K)) {
    
    N = n #number of samples
    U = runif(N) #N random samples from U(0,1)
    
    pi.mix.sum <- rep(0, length(pi.mix)+1)
    for (i in 2:length(pi.mix.sum)) pi.mix.sum[i] <- pi.mix.sum[i-1] + pi.mix[i-1]
    
    sample = rep(NA,N) #N samples from mixture model
    for (i in 1:N){
      
      k <- min(which(U[i]<pi.mix.sum))-1
      sample[i] = rnorm(1, mu.mix[k], sigma.mix[k])
    }
    
    return(list(sample=sample))
  }
  
  Cretlog <- seq(CretLogMin,CretLogMax,by=CretLogStep)
  Cret <- 10^Cretlog
  Wc <- array(NA, niter)
  Nportion <- matrix(NA, length(Cretlog),niter)
  Ptr <- array(NA, length(Cretlog))
  dose <- matrix(NA, length(Cretlog),niter)
  
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  

  transferM2K <- sample(abscissaMeat2Kitchenwareapprox, niter, prob=PMeat2Kitchenwareapprox, replace=TRUE)
  transferK2M <- sample(abscissaKitchenware2Meatapprox, niter, prob=PKitchenware2Meatapprox, replace=TRUE)
  transferM2H <- sample(abscissaMeat2Handsapprox, niter, prob=PMeat2Handsapprox, replace=TRUE)
  transferH2M <- sample(abscissaHands2Meatapprox, niter, prob=PHands2Meatapprox, replace=TRUE)
  
  
  # AU34
  transferEnv <- transferM2K*transferK2M/100
  # AU36
  transferHands <- transferM2H*transferH2M/10000
  
  Ptr <- transferEnv*rbinom(niter,1,probEnv)+(1-transferEnv)*transferHands*rbinom(niter,1,probHands)
  
  
  for (j in 1:length(Cretlog)){
    Nportion[j,1:niter] <- ifelse(Cret[j]*Wc<1e3, rpois(niter, Cret[j]*Wc), round(rnorm(niter, Cret[j]*Wc, sqrt(Cret[j]*Wc)),0))
    dose[j,1:niter] <- ifelse(Nportion[j,1:niter]==0, 
                              0, 
                              ifelse(Nportion[j,1:niter]<25e3, 
                                     rbinom(niter, size=Nportion[j,1:niter], prob=Ptr), 
                                     round(rnorm(niter,Nportion[j,1:niter]*Ptr,sqrt(Nportion[j,1:niter]*Ptr*(1-Ptr))))
                              )
    )
  }
  
  # exluding rare cases if normal distribution creates negativ doses
  dose<-ifelse(dose<0,0,dose)
  
  #calculating mean(dose) based on normal distribution of Cretlog
  require(distr)
  K <- length(muCret)
  CretLogDist <- rnorm.mixture(n=niter, K, mu.mix=muCret, sigma.mix=sigmaCret, pi.mix=piCret)$sample
  CretlogDens <- hist(CretLogDist[CretLogDist>CretLogMin & CretLogDist<CretLogMax],breaks=Cretlog,plot=FALSE)$density
  CretlogDens[length(Cretlog)]<-0
  newDistr <- DiscreteDistribution(supp=Cretlog,prob=CretlogDens/sum(CretlogDens))
  myNormCretlog <- r(newDistr)(1000)
  
  # weighted mean and probability of prevalence, based on normal distributed Cretlog
  dosemean <- mean(dose[match(myNormCretlog,Cretlog),1:niter])*Pprev
  PrevExp <- Pprev*(sum(dose[match(myNormCretlog,Cretlog),1:niter]>0)/niter/length(match(myNormCretlog,Cretlog)))
  return(list(Cretlog=Cretlog, dosemean=dosemean, dose=dose, PrevExp = PrevExp)) 
}

runConsumerPhaseCalistri <- modConsumerPhaseCalistri(niter, 
                                                     Pprev, 
                                                     muCret, sigmaCret, piCret,
                                                     CretLogMin, CretLogMax, CretLogStep, 
                                                     meanPortion, stdPortion, upperPortion, 
                                                     abscissaMeat2Kitchenware, ordinateMeat2Kitchenware, 
                                                     abscissaKitchenware2Meat, ordinateKitchenware2Meat, 
                                                     abscissaMeat2Hands, ordinateMeat2Hands, 
                                                     abscissaHands2Meat, ordinateHands2Meat, 
                                                     probHands, probEnv)
Cretlog <- runConsumerPhaseCalistri$Cretlog
dose <- runConsumerPhaseCalistri$dose
dosemean <- runConsumerPhaseCalistri$dosemean
PrevExp <- runConsumerPhaseCalistri$PrevExp
#############################




#################################
#visualisation
#################################
library(ggridges)
library(ggplot2)
library(viridis)

# if the user restricts the number of dose distributions shown:
# dhelp becomes a subset of dose
# chelp becomes a subset of Cretlog
dhelp <- dose
chelp <- Cretlog

totalNrOfDists <- length(Cretlog)

# take 1st and last Cretlog and a number of distributions in between
if (totalNrOfDists>maxDistsShown){
  totalNrOfDists = length(Cretlog)
  
  stepSize = round((totalNrOfDists-1)/(maxDistsShown-1))
  steps = seq(1,totalNrOfDists,by=stepSize)
  
  #consistency check
  if(length(steps)<maxDistsShown) maxDistsShown <- length(steps)
  
  steps[maxDistsShown] <- length(Cretlog)
  
  chelp <- Cretlog[steps]
  dhelp <- dose[steps,1:niter]
}
dvec <- as.vector(t(log10(dhelp+0.001)))
cvec <- sort(rep(chelp,niter))
d <- data.frame(cvec,dvec)
colnames(d)<-c("log of contamination at retail (log C_ret)","log of dose of contamination at consumer")
ggplot(d, aes(x = `log of dose of contamination at consumer`, y = `log of contamination at retail (log C_ret)`, fill = ..x.., group=`log of contamination at retail (log C_ret)`)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "color scale", option = "C") +
  labs(title = 'doses of campylobacter in chicken salad as function of contamination of chicken meat bought at retail')

library(gridExtra)
library(gridGraphics)
resultsCPM <- cbind(round(dosemean,0), round(PrevExp*100,1))
rownames(resultsCPM) <- myCPMname
colnames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]')


tableCPM <- matrix(round(dose,0))

rownames(tableCPM) <- rownames(tableCPM, do.NULL=FALSE, prefix="Value.")
colnames(tableCPM) <- c('Dose')
#write.csv(tableCPM, file="doses-consumer-phase-Nauta.csv")

pushViewport(viewport(x = .6, y = .2, height = .2, width = .2))    
grid.table(resultsCPM)										


#################################
#################################
#################################




#################################
#DRM
#################################
#parameters
#################################
# dose <- matrix(1,3,4)
 Pprev <- 0.25
  condPillinf <- 0.33
  betaGamma <- 7.59
  alphaGamma <- 0.145
  
  #################################  
  #model
  #################################
  modDoseResponseClassical <- function(dose, Pprev, condPillinf, alphaGamma, betaGamma){
    
    nrOfCrets <- dim(dose)[1]
    nrOfDoses <- dim(dose)[2]
    
    Pill <- matrix(NA, nrOfCrets, nrOfDoses)
    
    for (i in 1:nrOfDoses) Pill[1:nrOfCrets,i] <- condPillinf*round((1 - exp(lgamma(alphaGamma+betaGamma)+lgamma(betaGamma+dose[1:nrOfCrets,i])-lgamma(betaGamma)-lgamma(alphaGamma+betaGamma+dose[1:nrOfCrets,i]))),4)
    
    Pillmean <- rowMeans(Pill)
    Qill <- Pillmean*Pprev
    PrevExp <- Pprev*(sum(dose>0)/niter)
    
    return(list(Pill=Pill, Pillmean=Pillmean, Qill=Qill, PrevExp=PrevExp))
    #return(list(Qill=Qill, PrevExp=PrevExp))
  }
  
  runDoseResponseClassical <- modDoseResponseClassical(dose, Pprev, condPillinf, alphaGamma, betaGamma)
  
  
  Pillmean <-runDoseResponseClassical$Pillmean
  Pill <-runDoseResponseClassical$Pill
  PrevExp <- runDoseResponseClassical$PrevExp
  #################################
  
  
  #################################
  #visualisation
  #################################
#plot(Cretlog, Pillmean, xlab = "log(C_ret)", ylab = "P_ill", main = "probability of getting ill based on contamination of chicken meat bought at retail")
  #################################