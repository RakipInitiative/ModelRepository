#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,NaCl=NaCl,pH=pH,NaNO2=NaNO2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'NaCl') {
  multVar1 <- NaCl
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
if (visVar2 == 'NaCl') {
  multVar2 <- NaCl
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'NaNO2') {
  multVar2 <- NaNO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','NaCl','pH','NaNO2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('NaCl' %in% expectedAxes[is.na(notPresent)]) {myHash['NaCl'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
 
response_surface <- function(T,NaCl,pH,NaNO2) {
   26.0899-0.1901*T+0.0545*NaCl-6.3831*pH+0.0167*NaNO2+0.000201*T*NaCl+0.0000232*T*NaNO2-0.00729*NaCl*pH-0.00229*pH*NaNO2+0.0019*(T^2)+0.000098*(NaCl^2)+0.4784*(pH^2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
