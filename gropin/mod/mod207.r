#############################
# start of Model script
#############################
library(hash)
myHash <- hash(pH=pH,NaNO2=NaNO2,T=T,aw=aw)
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'aw') {
  multVar1 <- aw
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
if (visVar2 == 'aw') {
  multVar2 <- aw
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('pH','NaNO2','T','aw')
notPresent<-match(expectedAxes,visAxes)
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
 
response_surface <- function(pH,NaNO2,T,aw) {
   34.3435-4.4105*pH+0.0277*NaNO2-0.2272*T*aw-0.00366*pH*NaNO2+0.00319*(T^2)-13.9892*(aw^2)+0.3089*(pH^2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
