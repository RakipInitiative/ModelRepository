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

emptCE <- c(0, 0.001, 0.013, 0.02, 0.03, 0.05)
maxpCE <- 0.75
modepCE <- 0.50
cumPtCH <- c(0, 3.60, 53.60, 82.10, 92.90, 96.40, 100.00)/100
modepCH <- 0.2
emptER <- c(0, 0.1625, 0.325, 0.4875, 0.6, 0.65)
cumPtCE <- c(0, 4.40, 78.30, 91.30, 95.70, 100.00)/100
minpCH <- 0.1
emptHR <- c(0, 0.0041, 0.0621, 0.1, 0.12)
cumPtER <- c(21.10, 57.90, 73.70, 84.20, 89.50, 100.00)/100
minpCE <- 0.25
maxpCH <- 0.5
cumPtHR <- c(0, 16.70, 66.70, 83.30, 100.00)/100
emptCH <- c(0, 0.0017, 0.02, 0.05, 0.2, 0.24, 0.25)

maxDistsShown = 5
#############################


#################################
#model
#################################
# set.seed(1234)
myCPMname <- c("CPM Brynestad")
modConsumerPhaseBrynestad <- function(niter, 
                                      Pprev, 
                                      muCret, sigmaCret, piCret,
                                      CretLogMin, CretLogMax, CretLogStep, 
                                      meanPortion, stdPortion, upperPortion, 
                                      emptCE, cumPtCE, emptER, cumPtER, 
                                      emptCH, cumPtCH, emptHR, cumPtHR,  
                                      minpCE, maxpCE, modepCE, 
                                      minpCH, maxpCH, modepCH){
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
  #sampling from approximated empirical distribution
  napprox <- 1000
  
  emptCEapprox <- approx(emptCE, cumPtCE, n=napprox)$x
  cumPtCEapprox <- approx(emptCE, cumPtCE, n=napprox)$y
  PtCEapprox <- diff(c(0,cumPtCEapprox))
  
  emptERapprox <- approx(emptER, cumPtER, n=napprox)$x
  cumPtERapprox <- approx(emptER, cumPtER, n=napprox)$y
  PtERapprox <- diff(c(0,cumPtERapprox))
  
  emptCHapprox <- approx(emptCH, cumPtCH, n=napprox)$x
  cumPtCHapprox <- approx(emptCH, cumPtCH, n=napprox)$y
  PtCHapprox <- diff(c(0,cumPtCHapprox))
  
  emptHRapprox <- approx(emptHR, cumPtHR, n=napprox)$x
  cumPtHRapprox <- approx(emptHR, cumPtHR, n=napprox)$y
  PtHRapprox <- diff(c(0,cumPtHRapprox))
  
  
  Cretlog <- seq(CretLogMin,CretLogMax,by=CretLogStep)
  Cret <- 10^Cretlog
  Wc <- array(NA, niter)
  Nportion <- matrix(NA, length(Cretlog),niter)
  Ptr <- array(NA, length(Cretlog))
  dose <- matrix(NA, length(Cretlog),niter)
  
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
  tCE <- sample(emptCEapprox, niter, prob=PtCEapprox,replace=TRUE)
  tER <- sample(emptERapprox, niter, prob=PtERapprox,replace=TRUE)
  tCH <- sample(emptCHapprox, niter, prob=PtCHapprox,replace=TRUE)
  tHR <- sample(emptHRapprox, niter, prob=PtHRapprox,replace=TRUE)
  pCE <- rtriang(niter, min=minpCE, mode=modepCE, max=maxpCE)
  UfCE <- runif(niter)
  fCE <- ifelse(UfCE<pCE, 1, 0)
  pCH <- rtriang(niter, min=minpCH, mode=modepCH, max=maxpCH)
  UfCH <- runif(niter)
  fCH <- ifelse(UfCH<pCH, 1, 0)
  Ptr <- tCE*tER*fCE + (1-tCE*tER*fCE)*tCH*tHR*fCH
  
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

runConsumerPhaseBrynestad <- modConsumerPhaseBrynestad(niter, 
                                                       Pprev, 
                                                       muCret, sigmaCret, piCret,
                                                       CretLogMin, CretLogMax, CretLogStep, 
                                                       meanPortion, stdPortion, upperPortion, 
                                                       emptCE, cumPtCE, emptER, cumPtER, 
                                                       emptCH, cumPtCH, emptHR, cumPtHR,  
                                                       minpCE, maxpCE, modepCE, 
                                                       minpCH, maxpCH, modepCH)
Cretlog <- runConsumerPhaseBrynestad$Cretlog
dose <- runConsumerPhaseBrynestad$dose
dosemean <- runConsumerPhaseBrynestad$dosemean
PrevExp <- runConsumerPhaseBrynestad$PrevExp
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