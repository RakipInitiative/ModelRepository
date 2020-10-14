#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,Ac=Ac)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'Ac') {
  multVar1 <- Ac
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'Ac') {
  multVar2 <- Ac
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','Ac')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('Ac' %in% expectedAxes[is.na(notPresent)]) {myHash['Ac'] <- 0}
 
response_surface <- function(T,pH,Ac) {
   (0.046*(T+5.260)*sqrt(1-10^(3.793-c2))*sqrt(1-sqrt((Ac/(1+10^(pH-4.76)))/7.59)))^2/24
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
