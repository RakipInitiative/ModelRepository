#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,aw=aw,NaL=NaL,CO2_dissolved_=CO2_dissolved_)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'NaL') {
  multVar1 <- NaL
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
if (visVar2 == 'NaL') {
  multVar2 <- NaL
}
if (visVar2 == 'CO2_dissolved_') {
  multVar2 <- CO2_dissolved_
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','aw','NaL','CO2_dissolved_')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('NaL' %in% expectedAxes[is.na(notPresent)]) {myHash['NaL'] <- 0}
if('CO2_dissolved_' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2_dissolved_'] <- 0}
 
response_surface <- function(T,aw,NaL,CO2_dissolved_) {
   (Im+(m1*T)+(m2*aw)+(m3*NaL)+(m4*CO2_dissolved_)+(m5*(T^2))+(m6*(aw^2))+(m9*T*aw)+(m10*T*NaL)+(m11*T*CO2_dissolved_)+(m12*aw*NaL)+(m13*aw*NaL))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
