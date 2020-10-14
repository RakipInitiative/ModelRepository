#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,Oleo=Oleo)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'Oleo') {
  multVar1 <- Oleo
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'Oleo') {
  multVar2 <- Oleo
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','Oleo')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('Oleo' %in% expectedAxes[is.na(notPresent)]) {myHash['Oleo'] <- 0}
 
response_surface <- function(T,pH,Oleo) {
   (-5.348+0.162*T+0.319*pH+0.066*Oleo-0.0061*T*pH-0.00091*(T^2)-0.561*(Oleo^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
