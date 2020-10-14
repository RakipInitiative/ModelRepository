#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,NaL=NaL,aw=aw,CO2=CO2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'NaL') {
  multVar1 <- NaL
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'CO2') {
  multVar1 <- CO2
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'NaL') {
  multVar2 <- NaL
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'CO2') {
  multVar2 <- CO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','NaL','aw','CO2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('NaL' %in% expectedAxes[is.na(notPresent)]) {myHash['NaL'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('CO2' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2'] <- 0}
 
response_surface <- function(T,NaL,aw,CO2) {
   (ll+(l1*T)+(l2*aw)+(l3*NaL)+(l4*CO2)+(l5*(T^2))+(l7*(NaL^2))+(l9*T*aw)+(l10*T*NaL)+(l11*T*CO2)+(l12*aw*NaL)+(l14*NaL*CO2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
