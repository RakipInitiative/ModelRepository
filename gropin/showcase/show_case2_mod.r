#############################
# start of Model script
#############################
 
response_surface <- function(T) {
   (a*(T-Tmin))^2
} 
result <- response_surface(T)
if(mode=='time2multiply') {
  time2Xlog <- lagTime + logIncrease/result
}
if(mode=='kinetic') {
  q0 <- 1/(exp(lagTime)-1)
  mumax <- response_surface(T_kinetic)
  t <- seq(0,simTime,length.out = 71)
  A <- t + (1/mumax)*log((exp(-mumax*t)+q0)/(1+q0))
  logN = logN0 + mumax*A - log(1+((exp(mumax*A)-1)/(exp(logNEnd-logN0))))
}
#############################
# End of Model script
#############################
