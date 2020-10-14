#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,aw=aw,pH=pH,NaNO2=NaNO2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
 
if (visVar2 == 'T') {
  multVar2 <- T
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
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','aw','pH','NaNO2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
 
response_surface <- function(T,aw,pH,NaNO2) {
   252.833+0.1418*T-358.214*aw-18.4395*pH+0.0151*NaNO2-0.3653*T*aw+0.00452*T*pH+0.0000169*T*NaNO2+11.8359*aw*pH+0.00437*aw*NaNO2-0.00269*pH*NaNO2+0.00201*(T^2)+132.4864*(aw^2)+0.4881*(pH^2)+0.0000005*(NaNO2^2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
