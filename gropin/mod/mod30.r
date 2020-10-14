#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,aw=aw,CO2_dissolved_=CO2_dissolved_)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'CO2_dissolved_') {
  multVar1 <- CO2_dissolved_
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'CO2_dissolved_') {
  multVar2 <- CO2_dissolved_
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','aw','CO2_dissolved_')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('CO2_dissolved_' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2_dissolved_'] <- 0}
 
response_surface <- function(T,aw,CO2_dissolved_) {
   (Il+(I1*T)+(I2*CO2_dissolved_)+(I3*aw)+(I4*(T^2))+(I5*(CO2_dissolved_^2))+(I6*(aw^2))+(I7*T*CO2_dissolved_)+(I8*T*aw)+(I9*CO2_dissolved_*aw))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
