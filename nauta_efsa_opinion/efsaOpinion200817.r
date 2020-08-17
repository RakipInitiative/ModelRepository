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
doseAbsLogMax <- 7.7
doseAbsLog <- seq(doseAbsLogMin,doseAbsLogMax,by=doseAbsLogStep)

visding <- (1-Pprev)+Pprev*doseMCcdf(doseAbsLog)

plot(doseAbsLog,visding, type="l", lwd=3,xlab="log dose [cfu]",ylab = "cumulative probability", main="distribution of doses")

resultsCPM <- t(cbind(round(dosemean,0), round(PrevExp*100,1),Pprev,paste0("N(",muCret,", ",sigmaCret,")")))
colnames(resultsCPM) <- myCPMname
rownames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]','Pprev','input distribution log(C_ret)')

pushViewport(viewport(x = .5, y = .3, height = .2, width = .2))    
grid.table(resultsCPM)										


#################################
#################################
#################################
# DRM
#################################
#################################
#################################

#dose <- matrix(1,91,10000)
#Pprev <- 0.25
alphaGamma <- 0.44
betaGamma <- 0.51
rPillinf <- 0.06
etaPillinf <- 0.88
#Cretlog <- rep(1,91)


#################################
#model
#################################

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
plot(Cretlog, Pillmean, xlab = "log(C_ret)", ylab = "P_ill", main = "probability of getting ill based on contamination of chicken meat bought at retail")
#################################

#################################
#################################
#################################
# risk reduction
#################################
#################################
#################################

#Cretlog <- rep(1,91)
#Pillmean <- rep(1,91)
niter <- 250000
slopeMin <- 0.0
slopeML <- 0.27
slopeMax <- 0.7
tauMin <- 0.0
tauMax <- 3.0
scenarioMean <- 1.23
scenarioVar <- 0.0
scenarioUnc <- 0.66
# EU stuff
MS <- "MT"
#p <- 67.0
#mu <- 2.44
#sigma <- 1.24
#################################

library(MALDIquant)  
library(plyr)
library(mc2d)
#################################
# EFSA opinion model
#################################
# model 
#################################

# load from all Member states skin results 
dfMS <- read.table(paste0(dirname(sys.frame(1)$ofile),"/ms.csv"), 
                 header = TRUE,
                 sep = ";")


getMSROW <- which(dfMS == MS, arr.ind = TRUE)[1]

p <- dfMS$pMS[getMSROW]
mu <- dfMS$muMS[getMSROW]
sigma <- dfMS$sigmaMS[getMSROW]


# machine tolerance for numeric error, needed for matching double numbers
tol = .Machine$double.eps^0.5

# case correction from other models
CretLog <- Cretlog


CretSkin <- c(0.0001,0.0002,0.0005,
              0.001,0.002,0.005,
              0.01,0.02,0.05,
              0.1,0.2,0.5,
              1,2,5,
              10,20,50,
              100,200,500,
              1000,2000,5000,
              10000,20000,50000,
              100000,200000,500000,
              1000000,2000000,5000000,
              10000000,20000000,50000000)

CretLogSkin <- log10(CretSkin)


# user interface
slope <- rpert(niter,slopeMin,slopeML,slopeMax)
#slope <- 0.27
tau <- runif(1, min = tauMin, max = tauMax)
tau <- 1
deltaFec <- rnorm(niter,scenarioMean,scenarioUnc)


# DR plus
CretlogMeat <- CretLogSkin - tau

lowerLogRisk <- log10(Pillmean[match.closest(round_any(CretlogMeat, 0.1, f=floor),CretLog,tol=tol)])
lowerLogRisk <- ifelse(is.na(lowerLogRisk),0,lowerLogRisk)
upperLogRisk <- log10(Pillmean[match.closest(round_any(CretlogMeat, 0.1, f=ceiling),CretLog,tol=tol)])
upperLogRisk <- ifelse(is.na(upperLogRisk),0,upperLogRisk)

logRiskMod <- (lowerLogRisk-upperLogRisk)*(CretlogMeat%%0.1)*10
logRisk <- lowerLogRisk - logRiskMod


risk <- ifelse(CretlogMeat < -2, 0 , 10^logRisk)


#meat conc distributions
Pdose1 <- dnorm(CretLogSkin,mu,sigma)
delta <- diff(CretLogSkin)
logDoseInterval <- array(NA, length(CretLogSkin))
logDoseInterval[2:(length(CretLogSkin)-1)] <- (delta[1:length(delta)-1] + delta[2:length(delta)])/2

# Correction? in @Risk code even weirder
logDoseInterval[1] <- logDoseInterval[2]
logDoseInterval[length(CretLogSkin)] <- logDoseInterval[length(CretLogSkin)-1]

#H column
newVector <- Pdose1*logDoseInterval
Pdose <- newVector/sum(newVector)

# columns K through M
# here happens the actual simulation
muAfterIntervention <- mu - slope*deltaFec#
sigmaAfterIntervention <- sqrt(sigma^2 + scenarioVar^2*slope^2)#

Pdose1AfterIntervention <- matrix(NA,length(CretLogSkin),niter)
newVectorAfterIntervention <- matrix(NA,length(CretLogSkin),niter)

for(i in 1:niter) {
  Pdose1AfterIntervention[1:length(CretLogSkin),i] <- dnorm(CretLogSkin,muAfterIntervention[i],sigmaAfterIntervention[i])
}
newVectorAfterIntervention <- matrix(logDoseInterval,length(CretLogSkin),niter)*Pdose1AfterIntervention
PdoseAfterIntervention <- newVectorAfterIntervention/t(matrix(colSums(newVectorAfterIntervention),niter,length(CretLogSkin)))



#back to DR plus
RISK <- risk*Pdose
RISKAfterIntervention <- risk*PdoseAfterIntervention

RR <- ifelse(colSums(RISKAfterIntervention)>sum(RISK),1,colSums(RISKAfterIntervention)/sum(RISK))




#################################



#################################
#visualization
#################################
RRquant <- quantile(RR,c(.01,.025,.05,.5,.95,.975,.99))
RR2p5 <- 1-RRquant["2.5%"]
RR50 <- 1-RRquant["50%"]
RR97p5 <- 1-RRquant["97.5%"]
RRerrMinus <- RR50-RR2p5
RRerrPlus <- RR97p5-RR50
RRmean <- 1-mean(RR)
RRleft <- 1-mean(RR)+RRerrMinus
RRright <- 1-mean(RR)-RRerrPlus


RRdens <- density((1-RR))

plot(RRdens, type="l", lwd=3, ylab=" ",xlab="Risk Reduction", main="probability of Risk Reduction")
segments(RRleft,0, RRleft, RRdens$y[match.closest(RRleft,RRdens$x)], lwd=3, lty=2)
segments(RRright,0, RRright, RRdens$y[match.closest(RRright,RRdens$x)], lwd=3, lty=2)
segments(RRmean,0, RRmean, RRdens$y[match.closest(RRmean,RRdens$x)], lwd=3, lty=3)
text(RRmean-0.1, RRdens$y[match.closest(RRmean,RRdens$x)]/10, paste0("Mean RRR=", round_any(RRmean*100,0.1), "%"), pos=3)




#################################
