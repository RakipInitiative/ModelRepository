#############################
# start of Model script
#############################
library(hash)
myHash <- hash(CO2=CO2,NaCl=NaCl,NaNO2=NaNO2)
if (visVar1 == 'CO2') {
  multVar1 <- CO2
}
if (visVar1 == 'NaCl') {
  multVar1 <- NaCl
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
 
if (visVar2 == 'CO2') {
  multVar2 <- CO2
}
if (visVar2 == 'NaCl') {
  multVar2 <- NaCl
}
if (visVar2 == 'NaNO2') {
  multVar2 <- NaNO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('CO2','NaCl','NaNO2')
notPresent<-match(expectedAxes,visAxes)
if('CO2' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2'] <- 0}
if('NaCl' %in% expectedAxes[is.na(notPresent)]) {myHash['NaCl'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
 
response_surface <- function(CO2,NaCl,NaNO2) {
   1-0.4*NaNO2+0.3*CO2*NaNO2-0.5*(NaCl^2)-0.4*(CO2^2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
