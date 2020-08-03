
library(mc2d)

modConsumerPhaseMylius <- function(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, chicken2boardMean, chicken2boardStd, board2saladMean, board2saladStd, chicken2handsMean, chicken2handsStd, hands2saladMean, hands2saladStd, hands2handsMean, hands2handsStd, saladWashingEffectMin, saladWashingEffectMostLikely, saladWashingEffectMax, boardWashingEffectMin, boardWashingEffectMostLikely, boardWashingEffectMax, cuttingBoardHandlingUsesDifferentBoard, cuttingBoardHandlingTurnAroundBoard, cuttingBoardHandlingUseSameBoard, cuttingBoardHandlingCleanBoard, transferHands2tap, freqChickenFirs, freqHandWash, freqBoardWash, freqWashSalad, normalLimit){
  
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
    


    dose[i] <- ifelse(Nportion[i]==0, 0, rbinom(1, size=Nportion[i], prob=Ptr[i]))
  }
  
  dosemean <- mean(dose)*Pprev
  PrevExp <- Pprev*(sum(dose>0)/niter)
  
  return(list(dose=dose, dosemean=dosemean, PrevExp = PrevExp))
}

runConsumerPhaseMylius <- modConsumerPhaseMylius(niter, Pprev, muCret, sigmaCret, meanPortion, stdPortion, upperPortion, chicken2boardMean, chicken2boardStd, board2saladMean, board2saladStd, chicken2handsMean, chicken2handsStd, hands2saladMean, hands2saladStd, hands2handsMean, hands2handsStd, saladWashingEffectMin, saladWashingEffectMostLikely, saladWashingEffectMax, boardWashingEffectMin, boardWashingEffectMostLikely, boardWashingEffectMax, cuttingBoardHandlingUsesDifferentBoard, cuttingBoardHandlingTurnAroundBoard, cuttingBoardHandlingUseSameBoard, cuttingBoardHandlingCleanBoard, transferHands2tap, freqChickenFirs, freqHandWash, freqBoardWash, freqWashSalad, normalLimit)


dose <- runConsumerPhaseMylius$dose
dosemean <- runConsumerPhaseMylius$dosemean
PrevExp <- runConsumerPhaseMylius$PrevExp

