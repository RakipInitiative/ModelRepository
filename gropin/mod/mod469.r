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
   2*(((T+1.5)/(37+15))^2)*((pH-4.39)*(9.6-pH)/(7-4.39)*(9.6-7))*((aw-0.92)*(1-0.92))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################