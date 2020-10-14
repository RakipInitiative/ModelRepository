#############################
# start of Model script
#############################
library(hash)
myHash <- hash(S/S=S/S,Ac=Ac,pH=pH)
if (visVar1 == 'S/S') {
  multVar1 <- S/S
}
if (visVar1 == 'Ac') {
  multVar1 <- Ac
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
 
if (visVar2 == 'S/S') {
  multVar2 <- S/S
}
if (visVar2 == 'Ac') {
  multVar2 <- Ac
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('S/S','Ac','pH')
notPresent<-match(expectedAxes,visAxes)
if('S/S' %in% expectedAxes[is.na(notPresent)]) {myHash['S/S'] <- 0}
if('Ac' %in% expectedAxes[is.na(notPresent)]) {myHash['Ac'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
 
response_surface <- function(S/S,Ac,pH) {
   (4.53+3.29*((S/S-1.66)/0.6)+2.94*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)+0.36*(((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)^2)+0.82*((S/S-1.66)/0.6)*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
