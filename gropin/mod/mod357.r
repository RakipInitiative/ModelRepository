#############################
# start of Model script
#############################
library(hash)
myHash <- hash(pH=pH,aw=aw,Gelatin=Gelatin)
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'Gelatin') {
  multVar1 <- Gelatin
}
 
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'Gelatin') {
  multVar2 <- Gelatin
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('pH','aw','Gelatin')
notPresent<-match(expectedAxes,visAxes)
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('Gelatin' %in% expectedAxes[is.na(notPresent)]) {myHash['Gelatin'] <- 0}
 
response_surface <- function(pH,aw,Gelatin) {
   4.455*sqrt(aw-0.9488)*sqrt(1-10^(4.135-pH))*sqrt(0.6313+(1-0.6313)*(0.4027/(0.4027+Gelatin)))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
