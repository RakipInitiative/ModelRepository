#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,aw=aw,Phe=Phe,nit=nit,CO2=CO2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'Phe') {
  multVar1 <- Phe
}
if (visVar1 == 'nit') {
  multVar1 <- nit
}
if (visVar1 == 'CO2') {
  multVar1 <- CO2
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
if (visVar2 == 'Phe') {
  multVar2 <- Phe
}
if (visVar2 == 'nit') {
  multVar2 <- nit
}
if (visVar2 == 'CO2') {
  multVar2 <- CO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','aw','Phe','nit','CO2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('Phe' %in% expectedAxes[is.na(notPresent)]) {myHash['Phe'] <- 0}
if('nit' %in% expectedAxes[is.na(notPresent)]) {myHash['nit'] <- 0}
if('CO2' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2'] <- 0}
 
response_surface <- function(T,pH,aw,Phe,nit,CO2) {
   0.565*(((T-45.5)*((T+1.72)^2))/((37+1.72)*((37+1.72)*(T-37)-(37-45.5)*(37-1.72-2*T))))*(((pH-9.61)*(pH-4.71))/(((7.1-4.71)*(pH-7.1))-((7.1-9.61)*(4.71-pH))))*((aw-0.913)/(0.997-0.913))*(1-(nit/25))*(1-(Phe/31.9))*(1-(CO2/3.04))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])),as.double(values(myHash[notVisibleAxes[3]])),as.double(values(myHash[notVisibleAxes[4]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
