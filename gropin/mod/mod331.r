#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,aw=aw,La=La)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'La') {
  multVar1 <- La
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'La') {
  multVar2 <- La
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','aw','La')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('La' %in% expectedAxes[is.na(notPresent)]) {myHash['La'] <- 0}
 
response_surface <- function(T,pH,aw,La) {
   0.2345*(T-4.14)*(1-EXP(0.2636*(T-49.55)))*sqrt(aw-0.9508)*sqrt(1-10^(3.909-pH))*sqrt(1-10^(pH-8.860))*sqrt(1-La/(10.43*(1+10^(pH-3.86))))*sqrt(1-La/(995.5*(1+10^(3.86-pH))))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
