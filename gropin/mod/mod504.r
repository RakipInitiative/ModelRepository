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
   (-18.851+0.2409*T+4.2628*pH+6.36771*sqrt(1-aw)-0.00232*(T^2)-0.31377*(pH^2)-43.1241*((sqrt(1-aw))^2)+La*(-3.5*(10^-5)-3*(10^-7)*T+0.000009*pH-0.00014*sqrt(1-aw)-1.2*(10^-9)*La))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
