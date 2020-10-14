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
   0.64*(((T-30.85)*((T+3.36)^2))/(30.37*(30.37*(T-27.01)-(-3.84)*(23.65-2*T))))*(1/5.38)*((pH-4.79)*(9.43-pH)/3873.82)*((NaCl+62.24)*(62.24-NaCl))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
