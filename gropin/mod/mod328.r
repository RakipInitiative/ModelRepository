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
   4.5*(((T-45.6)*((T-4.7)^2))/((40.6-4.7)*((40.6-4.7)*(T-40.6)-(40.6-45.6)*(40.6+4.7-2*T))))*(((pH-9.33)*(pH-3.85))/((6.59-3.85)*(pH-6.59)-(6.59-9.33)*(3.85-pH)))*(((aw-1)*((aw-0.944)^2))/((0.997-0.944)*((0.997-0.944)*(aw-0.997)-(0.997-1)*(0.997+0.944-2*aw))))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
