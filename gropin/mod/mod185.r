#############################
# start of Model script
#############################
library(hash)
myHash <- hash(pH=pH,T=T,CO2=CO2)
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'CO2') {
  multVar1 <- CO2
}
 
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'CO2') {
  multVar2 <- CO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('pH','T','CO2')
notPresent<-match(expectedAxes,visAxes)
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('CO2' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2'] <- 0}
 
response_surface <- function(pH,T,CO2) {
   a1+(a2*pH)+(a3*T)+(a4*CO2)+(a5*pH*T)+(a6*pH*CO2)+(a7*T*CO2)+(a8*(T^2))+(a9*(CO2^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
