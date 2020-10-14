#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,NaCl=NaCl)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'NaCl') {
  multVar1 <- NaCl
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
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','NaCl')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('NaCl' %in% expectedAxes[is.na(notPresent)]) {myHash['NaCl'] <- 0}
 
response_surface <- function(T,pH,NaCl) {
   152.04-(93751.2/(T+273))-1.64*NaCl+5.49*pH+(12593197/((T+273)^2))-0.06*(NaCl^2)-0.39*(pH^2)+(204.27*NaCl/(T+273))+0.13*pH*NaCl
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
