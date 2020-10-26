#############################
# start of Parameter script
#############################
T <- seq(4,12,length.out=21)
aw <- seq(0.974,0.992,length.out=21)
CO2dissolved <- seq(0,2411,length.out=21)
b <- 1.17e-05
awmin <- 0.9715
Tmin <- -5.68
CO2max <- 2300
visVar1 <- 'T'
visVar2 <- 'aw'
#mode <-'responsesurface'
#mode <-'time2multiply'
mode <-'kinetic'
lagTime <- 10.0
logIncrease <- 1.0
logN0 <- 0.0
logNEnd <- 10.0
simTime <- 1000.0
T_kinetic <- 8
aw_kinetic <- 0.983
CO2dissolved_kinetic <- 1205.5
#############################
# end of Parameter script
#############################
