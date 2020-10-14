#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,NaCl=NaCl,NaNO2=NaNO2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'NaCl') {
  multVar1 <- NaCl
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'NaCl') {
  multVar2 <- NaCl
}
if (visVar2 == 'NaNO2') {
  multVar2 <- NaNO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','NaCl','NaNO2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('NaCl' %in% expectedAxes[is.na(notPresent)]) {myHash['NaCl'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
 
response_surface <- function(T,pH,NaCl,NaNO2) {
   LN(2)/EXP(13.5303-0.1096*T+0.0266*NaCl-3.2997*pH+0.0232*NaNO2-0.0134*T*pH-0.00332*NaCl*pH-0.00296*pH*NaNO2+0.00271*(T^2)+0.257*(pH^2)-0.00000085*(NaNO2^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
