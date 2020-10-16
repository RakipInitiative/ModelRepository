#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,aw=aw,CO2dissolved=CO2dissolved)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'CO2dissolved') {
  multVar1 <- CO2dissolved
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'CO2dissolved') {
  multVar2 <- CO2dissolved
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','aw','CO2dissolved')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('CO2dissolved' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2dissolved'] <- 0}
 
response_surface <- function(T,aw,CO2dissolved) {
   (b*(aw-awmin)*(CO2max-CO2dissolved)*((T-Tmin)^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
if(mode=='time2multiply') {
  time2Xlog <- lagTime + logIncrease/result
}
if(mode=='kinetic') {
  time2Xlog <- lagTime + logIncrease/result
  q0 <- 1/(exp(lagTime)-1)
  mumax <- response_surface(T_kinetic,aw_kinetic,CO2dissolved_kinetic)
  t <- seq(0,simTime,length.out = 71)
  A <- t + (1/mumax)*log((exp(-mumax*t)+q0)/(1+q0))
  logN = logN0 + mumax*A - log(1+((exp(mumax*A)-1)/(exp(logNEnd-logN0))))
}
#############################
# End of Model script
#############################
