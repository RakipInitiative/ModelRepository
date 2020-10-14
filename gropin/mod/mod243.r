#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,aw=aw)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'aw') {
  multVar1 <- aw
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
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','aw')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
 
response_surface <- function(T,pH,aw) {
   mopt_days^-1_*((aw-awmin)/(1-awmin))*((((T-Tmin_oC_)^2)*(T-Tmax_oC_))/((Topt_oC_-Tmin_oC_)*((Topt_oC_-Tmin_oC_)*(T-Topt_oC_)-(Topt_oC_-Tmax_oC_)*(Topt_oC_+Tmin_oC_-2*T))))*((((pH-pHmin)^2)*(pH-pHmax))/((pHopt-pHmin)*((pHopt-pHmin)*(pH-pHopt)-(pHopt-pHmax)*(pHopt+pHmin-2*pH))))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
