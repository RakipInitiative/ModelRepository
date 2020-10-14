#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,CO2_dissolved_=CO2_dissolved_,aw=aw,NaL=NaL)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'CO2_dissolved_') {
  multVar1 <- CO2_dissolved_
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'NaL') {
  multVar1 <- NaL
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'CO2_dissolved_') {
  multVar2 <- CO2_dissolved_
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'NaL') {
  multVar2 <- NaL
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','CO2_dissolved_','aw','NaL')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('CO2_dissolved_' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2_dissolved_'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('NaL' %in% expectedAxes[is.na(notPresent)]) {myHash['NaL'] <- 0}
 
response_surface <- function(T,CO2_dissolved_,aw,NaL) {
   (1/(b*(aw-awmin)*(CO2max-CO2_dissolved_)*(T-Tmin)*(NaLmax-NaL)))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
