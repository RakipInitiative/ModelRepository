#############################
# start of Model script
#############################
library(hash)
myHash <- hash(aw=aw,pH=pH,NaNO2=NaNO2,T=T)
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
if (visVar1 == 'T') {
  multVar1 <- T
}
 
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'NaNO2') {
  multVar2 <- NaNO2
}
if (visVar2 == 'T') {
  multVar2 <- T
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('aw','pH','NaNO2','T')
notPresent<-match(expectedAxes,visAxes)
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
 
response_surface <- function(aw,pH,NaNO2,T) {
   248.1902-342.9966*aw-18.9842*pH+0.0169*NaNO2-0.192*T*aw+0.0000215*T*NaNO2+12.5454*aw*pH-0.00231*pH*NaNO2+0.00203*(T^2)+121.0776*(aw^2)+0.4816*(pH^2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
