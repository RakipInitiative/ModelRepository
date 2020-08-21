niter <- 10000
Pprev <- 0.25
muCret <- 1.5
sigmaCret <- 1.2
piCret <- 1.0
meanPortion <- 189.0
stdPortion <- 126.9
upperPortion <- 1000.0
logPtr <- c(2.24, 2.36, 2.37, 2.58, 2.82, 2.86, 3.16, 3.17, 3.47, 3.52, 3.57, 3.83, 3.83, 3.84, 3.87, 3.89, 3.89, 3.90, 3.94, 4.03, 4.09, 4.42, 4.53, 4.54, 4.54, 4.62, 4.62, 4.68, 4.73, 4.76,4.84, 4.92, 4.93, 4.95, 4.97, 5.20, 5.25, 5.27, 5.39, 5.47, 5.60, 5.83, 5.89, 5.95, 5.96, 6.02, 6.23, 6.38, 6.96, 7.37, 7.90, 8.20, 9.00, 9.00, 9.00)
CretLogMin <- -2.0
CretLogMax <- 7.0
CretLogStep <- 0.1
maxDistsShown <- 7


require(distr)
#################################
#model
#################################
# set.seed(1234)
myCPMname <- c("CPM Nauta")
modConsumerPhaseNauta <- function(niter, 
                                  Pprev, 
                                  muCret, sigmaCret, piCret,
                                  CretLogMin, CretLogMax, CretLogStep, 
                                  meanPortion, stdPortion, upperPortion, 
                                  logPtr){
  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)
  
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
  
  # initialising arrays
  Cretlog <- seq(CretLogMin,CretLogMax,by=CretLogStep)
  Cret <- 10^Cretlog
  Wc <- array(NA, niter)
  Nportion <- matrix(NA, length(Cretlog),niter)
  Ptr <- array(NA, length(Cretlog))
  dose <- matrix(NA, length(Cretlog),niter)
  
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
  # transfer rate
  Ptr <- 10^(-sample(logPtr, niter,replace=TRUE))
  
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
  
  K <- length(muCret)
  CretLogDist <- rnorm.mixture(n=niter, K, mu.mix=muCret, sigma.mix=sigmaCret, pi.mix=piCret)$sample
  CretlogDens <- hist(CretLogDist[CretLogDist>CretLogMin & CretLogDist<CretLogMax],breaks=Cretlog,plot=FALSE)$density
  CretlogDens[length(Cretlog)]<-0
  newDistr <- DiscreteDistribution(supp=Cretlog,prob=CretlogDens/sum(CretlogDens))
  myNormCretlog <- r(newDistr)(1000)
  
  # weighted mean and probability of prevalence, based on normal distributed Cretlog
  doseMC <- as.vector(dose[match(myNormCretlog,Cretlog),1:niter])
  dosemean <- mean(doseMC)*Pprev
  PrevExp <- Pprev*(sum(doseMC>0)/niter/length(match(myNormCretlog,Cretlog)))
  
  return(list(Cretlog=Cretlog, dosemean=dosemean, dose=dose, doseMC=doseMC, PrevExp = PrevExp)) 
}

runConsumerPhaseNauta <- modConsumerPhaseNauta(niter, 
                                               Pprev, 
                                               muCret, sigmaCret, piCret,
                                               CretLogMin, CretLogMax, CretLogStep, 
                                               meanPortion, stdPortion, upperPortion, 
                                               logPtr)

Cretlog <- runConsumerPhaseNauta$Cretlog
dose <- runConsumerPhaseNauta$dose
dosemean <- runConsumerPhaseNauta$dosemean
doseMC <- runConsumerPhaseNauta$doseMC
PrevExp <- runConsumerPhaseNauta$PrevExp
#############################


library(gridExtra)
library(gridGraphics)
#################################
#visualisation
#################################
doseMCcdf <- ecdf(doseMC)
doseAbsLogMin <- 0
doseAbsLogStep <- 0.01
doseAbsLogMax <- 8
doseAbsLog <- seq(doseAbsLogMin,doseAbsLogMax,by=doseAbsLogStep)

visding <- (1-Pprev)+Pprev*doseMCcdf(10^(doseAbsLog))

plot(doseAbsLog,visding, type="l", lwd=3,xlab="log dose [cfu]",ylab = "cumulative probability", main="distribution of doses")

resultsCPM <- t(cbind(round(dosemean,0), round(PrevExp*100,1),Pprev,paste0("N(",muCret,", ",sigmaCret,")")))
colnames(resultsCPM) <- myCPMname
rownames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]','Pprev','input distribution log(C_ret)')

pushViewport(viewport(x = .5, y = .4, height = .2, width = .2))    
grid.table(resultsCPM)										

