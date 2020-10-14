#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,CO2=CO2,O2=O2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'CO2') {
  multVar1 <- CO2
}
if (visVar1 == 'O2') {
  multVar1 <- O2
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'CO2') {
  multVar2 <- CO2
}
if (visVar2 == 'O2') {
  multVar2 <- O2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','CO2','O2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('CO2' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2'] <- 0}
if('O2' %in% expectedAxes[is.na(notPresent)]) {myHash['O2'] <- 0}
 
response_surface <- function(T,pH,CO2,O2) {
   (-4.50+(-0.829)*T+(-0.0151)*(T^2)+(-0.00122)*T*CO2+0.184*T*pH+(-0.00114)*pH*O2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
