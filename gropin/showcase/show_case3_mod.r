#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- days
 
response_surface <- function(T,days) {
   0.89+0.081*days+0.11*T+0.000192*(days^2)-0.0034*(T^2)+0.0068*T*days
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
if(mode=='time2multiply') {
  time2Xlog <- lagTime + logIncrease/result
}
if(mode=='kinetic') {
  time2Xlog <- lagTime + logIncrease/result
  q0 <- 1/(exp(lagTime)-1)
  mumax <- response_surface(T_kinetic,days_kinetic)
  t <- seq(0,simTime,length.out = 71)
  A <- t + (1/mumax)*log((exp(-mumax*t)+q0)/(1+q0))
  logN = logN0 + mumax*A - log(1+((exp(mumax*A)-1)/(exp(logNEnd-logN0))))
}
#############################
# End of Model script
#############################
