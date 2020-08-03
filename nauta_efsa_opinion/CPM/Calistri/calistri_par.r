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

