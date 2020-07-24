meanPortion <- 189.0
stdPortion <- 126.9
muCret <- 1.5
sigmaCret <- 0.2
Pprev <- 0.25
niter <- 100000
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

# K3
normalLimit <- 25000


library(mc2d)

#modConsumerPhaseMylius <- function(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, 
 #                                   ){
  
  #transform lognormal parameters for the portion size
  fvarWc <- function(x,a) (exp(x) - 1)*exp(2*log(meanPortion))-stdPortion^2
  sigmaWc <- round(sqrt(uniroot(fvarWc, lower = 0, upper = 1e3, tol = 1e-7)$root),3)
  muWc <- round(log(meanPortion) - ((sigmaWc)^2)/2,3)

  #std input
  Cretlog <- array(NA, niter)
  Cret <- array(NA, niter)
  Wc <- array(NA, niter)
  Nportion <- array(NA, niter)
  Ptr <- array(NA, niter)
  dose <- array(NA, niter)
  
  
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
  
    
  for (i in 1:niter){
    
  ############################################################################
  # Reference given in @Risk code: Smith JFP 66 2003 2359-2361
  # N8
  ############################################################################
  saladWashingEffectProb <- 10^-(rpert(1,saladWashingEffectMin,saladWashingEffectMostLikely,saladWashingEffectMax))
  ############################################################################
  
  ############################################################################
  # Reference given in @Risk code: Cogan eta al 2002 
  # N16
  ############################################################################
  boardWashingEffectProb <- 10^-(rpert(1,boardWashingEffectMin,boardWashingEffectMostLikely,boardWashingEffectMax))
  ############################################################################

  
  ############################################################################
  #cross contamination probabilities
  ############################################################################
  # Montville et al
  chicken2handsProb <- min(rbeta(1,chicken2handsMean,chicken2handsStd),1)
  hands2saladProb <- min(rbeta(1,hands2saladMean,hands2saladStd),1)
   
  # Kusumaningrum et al
  chicken2boardProb <- min(10^(rnorm(1,chicken2boardMean,chicken2boardStd))/100,1-chicken2handsProb)
  board2saladProb <- min(10^(rnorm(1,board2saladMean,board2saladStd))/100,1)
  
  
  
  # Chen
  hands2handsProb <- min(rbeta(1,hands2handsMean,hands2handsStd),1)
  ############################################################################
   
  
  
  ############################################################################
  # probability of transfer or survival (t_xy) B4
  ############################################################################
  transferHands2hands <- hands2handsProb  #D7
  transferHands2salad <- min(hands2saladProb, 0.995 - transferHands2hands - transferHands2tap) #D8
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
  effectWashboardSurvivalProb <- ifelse(rbinom(1,1,probOfWashing)==1, transferBoard2board, 0) #C21
  happenWashingBoard <- rbinom(1,1,freqBoardWash) #C22
  finalSurvivalProbWashingBoard <- ifelse(happenWashingBoard<0.5, 1, effectWashboardSurvivalProb) #C23
 
  effectWashingHandsSurvivalProb <- transferHands2hands #D21
  happenWashingHands <- rbinom(1,1,freqHandWash) #D22
  finalSurvivalProbWashingHands <- ifelse(happenWashingHands<0.5, 1, effectWashingHandsSurvivalProb) #D23
  
  effectCutSaladSurvivalProb <- 1 #E21
  happenCutSalad <- rbinom(1,1,freqChickenFirs) #E22
  finalSurvivalProbCutSalad <- happenCutSalad # E23
  
  effectWashingSaladSurvivalProb <- transferSalad2salad #F21
  happenWashingSalad <- rbinom(1,1,freqWashSalad)*finalSurvivalProbCutSalad #F22
  finalSurvivalProbWashingSalad <- ifelse(happenWashingSalad<0.5, 1, effectWashingSaladSurvivalProb) #F23
  ############################################################################
  
  
  
  
  ############################################################################
  # transfer to salad (from chicken to salad, considering washing) #B25
  ############################################################################
  transferChicken2boardAfterWashing <- transferChicken2board*finalSurvivalProbWashingBoard #D27
  transferChicken2handsAfterWashing <- transferChicken2hands*finalSurvivalProbWashingHands #D28
  transferChicken2saladAfterWashing <- (transferBoard2salad*transferChicken2boardAfterWashing+transferHands2salad*transferChicken2handsAfterWashing)*finalSurvivalProbCutSalad #E29
  ############################################################################

  Ptr[i] <- transferChicken2saladAfterWashing*finalSurvivalProbWashingSalad
  
    Cretlog[i] <- rnorm(1, muCret, sigmaCret)
    Cret[i] <- 10^Cretlog[i]
    
    Wc[i] <- min(rlnorm(1, muWc, sigmaWc), upperPortion)
    
    Nportion[i] <- ifelse(Cret[i]*Wc[i]<1e3, rpois(1, Cret[i]*Wc[i]), round(rnorm(1, Cret[i]*Wc[i], sqrt(Cret[i]*Wc[i])),0))
    


    dose[i] <- ifelse(Nportion[i]==0, 0, rbinom(1, size=Nportion[i], prob=Ptr))
  }
  
  dosemean <- mean(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  
  #return(list(dose=dose, dosemean=dosemean, PrevExp = PrevExp))
#}

#runConsumerPhaseMylius <- modConsumerPhaseMylius(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, 
                                                     #)


  #dose <- runConsumerPhaseMylius$dose
#dosemean <- runConsumerPhaseMylius$dosemean
#PrevExp <- runConsumerPhaseMylius$PrevExp



# visualization
library(gridExtra)

resultsCPM <- cbind(round(dosemean,0), round(PrevExp*100,1))
rownames(resultsCPM) <- c("CPM Mylius")
colnames(resultsCPM) <- c("Mean dose", 'Prevalence of exposure [%]')

tableCPM <- matrix(round(dose,0))
rownames(tableCPM) <- rownames(tableCPM, do.NULL=FALSE, prefix="Value.")
colnames(tableCPM) <- c('Dose')
write.csv(tableCPM, file="doses-consumer-phase-Mylius.csv")

sortdose <-sort(log10(dose))
vsize <-length(sortdose)
pdose <- array(NA,vsize)
for (i in 1:vsize)
{pdose[i] <- (i-0.5)/vsize}

plot(sortdose,pdose, 
     xlab = "log Dose", ylab = "",
     main = "doses from meat contaminated at retail")
grid.table(resultsCPM)