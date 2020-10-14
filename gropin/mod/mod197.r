#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,NaCl=NaCl,pH=pH,NaNO2=NaNO2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'NaCl') {
  multVar1 <- NaCl
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'NaCl') {
  multVar2 <- NaCl
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'NaNO2') {
  multVar2 <- NaNO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','NaCl','pH','NaNO2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('NaCl' %in% expectedAxes[is.na(notPresent)]) {myHash['NaCl'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
 
response_surface <- function(T,NaCl,pH,NaNO2) {
   19.8265-(0.2028*T)+(0.0314*NaCl)-(4.3495*pH)+(0.0245*NaNO2)+(0.0000706*T*NaCl)-(0.0032*T*pH)+(0.0000162*T*NaNO2)-(0.00181*NaCl*pH)-(0.00000265*NaCl*NaNO2)-(0.00321*pH*NaNO2)+(0.00312*(T^2))+(0.0000258*(NaCl^2))+(0.3105*(pH^2))-(0.00000048*(NaNO2^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
