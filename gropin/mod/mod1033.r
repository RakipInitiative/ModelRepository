#############################
# start of Model script
#############################
library(hash)
myHash <- hash(Fructose=Fructose,Ethanol=Ethanol,pH=pH)
if (visVar1 == 'Fructose') {
  multVar1 <- Fructose
}
if (visVar1 == 'Ethanol') {
  multVar1 <- Ethanol
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
 
if (visVar2 == 'Fructose') {
  multVar2 <- Fructose
}
if (visVar2 == 'Ethanol') {
  multVar2 <- Ethanol
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('Fructose','Ethanol','pH')
notPresent<-match(expectedAxes,visAxes)
if('Fructose' %in% expectedAxes[is.na(notPresent)]) {myHash['Fructose'] <- 0}
if('Ethanol' %in% expectedAxes[is.na(notPresent)]) {myHash['Ethanol'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
 
response_surface <- function(Fructose,Ethanol,pH) {
   LN(0.45)-0.0146*((pH-7)^2)-40.85*((sqrt(1-( 55.556/(55.556+(Ethanol*0.7893/46)+(Fructose*10/180.16))))-0.05)^2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
