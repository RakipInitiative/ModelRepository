# CPM
#################################
#parameters
#################################
niter <- 5000
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

#cross contamination
# from Kusumaninggrum et al J8:K9
chicken2boardMean <- 0.098
chicken2boardStd <- 0.606
board2saladMean <- 1.535
board2saladStd <- 0.32

#from Montville et al 2001 J12:K13
chicken2handsMean <- 1.78
chicken2handsStd <- 41.1
hands2saladMean <- 0.6
hands2saladStd <- 2.3

# from Chen J15:K15
hands2handsMean <- 0.24
hands2handsStd <- 6.67

# from Smith et al 2003 N5:O5
saladWashingEffectMin <- 0
saladWashingEffectMostLikely <- 0.4
saladWashingEffectMax <- 1

# from Cogan eta al 2002 N13:P13
boardWashingEffectMin <- 1
boardWashingEffectMostLikely <- 4.5
boardWashingEffectMax <- 7

# Board handling N19:N22
cuttingBoardHandlingUsesDifferentBoard <- 1081
cuttingBoardHandlingTurnAroundBoard <- 218
cuttingBoardHandlingUseSameBoard <- 103
cuttingBoardHandlingCleanBoard <- 1809

#D10
transferHands2tap <- 0.0016

#Frequencies C13:C16
freqChickenFirs <- 1
freqHandWash <- 0.8
freqBoardWash <- 0.95
freqWashSalad <- 0.6


maxDistsShown = 5


#############################


#################################
#model
#################################
library(mc2d)
# set.seed(1234)

myCPMname <- c("CPM Mylius")
modConsumerPhaseMylius <- function(niter, 
                                   Pprev, 
                                   muCret, sigmaCret, piCret,
                                   CretLogMin, CretLogMax, CretLogStep, 
                                   meanPortion, stdPortion, upperPortion, 
                                   chicken2boardMean, chicken2boardStd, 
                                   board2saladMean, board2saladStd, 
                                   chicken2handsMean, chicken2handsStd, 
                                   hands2saladMean, hands2saladStd, 
                                   hands2handsMean, hands2handsStd, 
                                   saladWashingEffectMin, saladWashingEffectMostLikely, saladWashingEffectMax, 
                                   boardWashingEffectMin, boardWashingEffectMostLikely, boardWashingEffectMax, 
                                   cuttingBoardHandlingUsesDifferentBoard, 
                                   cuttingBoardHandlingTurnAroundBoard, 
                                   cuttingBoardHandlingUseSameBoard, 
                                   cuttingBoardHandlingCleanBoard, 
                                   transferHands2tap, 
                                   freqChickenFirs, freqHandWash, freqBoardWash, freqWashSalad){
  
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
  
  Cretlog <- seq(CretLogMin,CretLogMax,by=CretLogStep)
  Cret <- 10^Cretlog
  Wc <- array(NA, niter)
  Nportion <- matrix(NA, length(Cretlog),niter)
  Ptr <- array(NA, length(Cretlog))
  dose <- matrix(NA, length(Cretlog),niter)
  
  ############################################################################
  # board handling probabilities N18
  ############################################################################
  cuttingBoardHandlingTotal <- cuttingBoardHandlingUsesDifferentBoard + cuttingBoardHandlingTurnAroundBoard + cuttingBoardHandlingUseSameBoard + cuttingBoardHandlingCleanBoard
  probCuttingBoardHandlingUsesDifferentBoard <- cuttingBoardHandlingUsesDifferentBoard/cuttingBoardHandlingTotal
  probCuttingBoardHandlingTurnAroundBoard <- cuttingBoardHandlingTurnAroundBoard/cuttingBoardHandlingTotal
  probCuttingBoardHandlingUseSameBoard <- cuttingBoardHandlingUseSameBoard/cuttingBoardHandlingTotal
  probCuttingBoardHandlingCleanBoard <- cuttingBoardHandlingCleanBoard/cuttingBoardHandlingTotal
  probOfWashing <- (probCuttingBoardHandlingTurnAroundBoard+probCuttingBoardHandlingCleanBoard)/(probCuttingBoardHandlingUsesDifferentBoard+probCuttingBoardHandlingTurnAroundBoard+probCuttingBoardHandlingCleanBoard)
  ############################################################################
  
  
  
  ############################################################################
  # Reference given in @Risk code: Smith JFP 66 2003 2359-2361
  # N8
  ############################################################################
  saladWashingEffectProb <- 10^-(rpert(niter,saladWashingEffectMin,saladWashingEffectMostLikely,saladWashingEffectMax))
  ############################################################################
  
  ############################################################################
  # Reference given in @Risk code: Cogan eta al 2002 
  # N16
  ############################################################################
  boardWashingEffectProb <- 10^-(rpert(niter,boardWashingEffectMin,boardWashingEffectMostLikely,boardWashingEffectMax))
  ############################################################################
  
  
  ############################################################################
  #cross contamination probabilities
  ############################################################################
  # Montville et al
  chicken2handsProb <- apply(cbind(rbeta(niter,chicken2handsMean,chicken2handsStd),rep(1,niter)),1,min)
  hands2saladProb <- apply(cbind(rbeta(niter,hands2saladMean,hands2saladStd),rep(1,niter)),1,min)

  
  # Kusumaningrum et al
  chicken2boardProb <- apply(cbind(10^(rnorm(niter,chicken2boardMean,chicken2boardStd))/100,rep(1-chicken2handsProb,niter)),1,min)
  board2saladProb <- apply(cbind(10^(rnorm(niter,board2saladMean,board2saladStd))/100,rep(1,niter)),1,min)
  
  
  # Chen
  hands2handsProb <- apply(cbind(rbeta(niter,hands2handsMean,hands2handsStd),rep(1,niter)),1,min)
  ############################################################################
  
  
  
  ############################################################################
  # probability of transfer or survival (t_xy) B4
  ############################################################################
  transferHands2hands <- hands2handsProb  #D7
  transferHands2salad <-  apply(cbind(hands2saladProb, 0.995 - transferHands2hands - transferHands2tap),1,min)
  transferChicken2hands <- chicken2handsProb #C7
  transferChicken2board <- chicken2boardProb #C9
  transferSalad2salad <- saladWashingEffectProb #E8
  transferBoard2salad <- board2saladProb #F8
  transferBoard2board <- boardWashingEffectProb #F9
  ############################################################################
  
  
  
  
  
  ############################################################################
  # Effect of washing and cutting #B18
  ############################################################################
  # C20:F20 are input parameters: freqBoardWash, freqHandWash, freqChickenFirs, freqWashSalad
  effectWashboardSurvivalProb <- ifelse(rbinom(niter,1,probOfWashing)==1, transferBoard2board, 0) #C21
  happenWashingBoard <- rbinom(niter,1,freqBoardWash) #C22
  finalSurvivalProbWashingBoard <- ifelse(happenWashingBoard<0.5, 1, effectWashboardSurvivalProb) #C23
  
  effectWashingHandsSurvivalProb <- transferHands2hands #D21
  happenWashingHands <- rbinom(niter,1,freqHandWash) #D22
  finalSurvivalProbWashingHands <- ifelse(happenWashingHands<0.5, 1, effectWashingHandsSurvivalProb) #D23
  
  effectCutSaladSurvivalProb <- 1 #E21
  happenCutSalad <- rbinom(niter,1,freqChickenFirs) #E22
  finalSurvivalProbCutSalad <- happenCutSalad # E23
  
  effectWashingSaladSurvivalProb <- transferSalad2salad #F21
  happenWashingSalad <- rbinom(niter,1,freqWashSalad)*finalSurvivalProbCutSalad #F22
  finalSurvivalProbWashingSalad <- ifelse(happenWashingSalad<0.5, 1, effectWashingSaladSurvivalProb) #F23
  ############################################################################
  
  
  
  
  ############################################################################
  # transfer to salad (from chicken to salad, considering washing) #B25
  ############################################################################
  transferChicken2boardAfterWashing <- transferChicken2board*finalSurvivalProbWashingBoard #D27
  transferChicken2handsAfterWashing <- transferChicken2hands*finalSurvivalProbWashingHands #D28
  transferChicken2saladAfterWashing <- (transferBoard2salad*transferChicken2boardAfterWashing+transferHands2salad*transferChicken2handsAfterWashing)*finalSurvivalProbCutSalad #E29
  ############################################################################
  
  Ptr <- transferChicken2saladAfterWashing*finalSurvivalProbWashingSalad
  
  
  
  
  
  Wc <- apply(cbind(rlnorm(niter, muWc, sigmaWc), upperPortion*rep(1,niter)),1,min)
  
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

runConsumerPhaseMylius <- modConsumerPhaseMylius(niter, 
                                                 Pprev, 
                                                 muCret, sigmaCret, piCret,
                                                 CretLogMin, CretLogMax, CretLogStep, 
                                                 meanPortion, stdPortion, upperPortion, 
                                                 chicken2boardMean, chicken2boardStd, 
                                                 board2saladMean, board2saladStd, 
                                                 chicken2handsMean, chicken2handsStd, 
                                                 hands2saladMean, hands2saladStd, 
                                                 hands2handsMean, hands2handsStd, 
                                                 saladWashingEffectMin, saladWashingEffectMostLikely, saladWashingEffectMax, 
                                                 boardWashingEffectMin, boardWashingEffectMostLikely, boardWashingEffectMax, 
                                                 cuttingBoardHandlingUsesDifferentBoard, 
                                                 cuttingBoardHandlingTurnAroundBoard, 
                                                 cuttingBoardHandlingUseSameBoard, 
                                                 cuttingBoardHandlingCleanBoard, 
                                                 transferHands2tap, 
                                                 freqChickenFirs, freqHandWash, freqBoardWash, freqWashSalad)
Cretlog <- runConsumerPhaseMylius$Cretlog
dose <- runConsumerPhaseMylius$dose
dosemean <- runConsumerPhaseMylius$dosemean
PrevExp <- runConsumerPhaseMylius$PrevExp
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