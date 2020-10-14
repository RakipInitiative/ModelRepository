#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,Phe=Phe,NaCl=NaCl)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'Phe') {
  multVar1 <- Phe
}
if (visVar1 == 'NaCl') {
  multVar1 <- NaCl
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'Phe') {
  multVar2 <- Phe
}
if (visVar2 == 'NaCl') {
  multVar2 <- NaCl
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','Phe','NaCl')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('Phe' %in% expectedAxes[is.na(notPresent)]) {myHash['Phe'] <- 0}
if('NaCl' %in% expectedAxes[is.na(notPresent)]) {myHash['NaCl'] <- 0}
 
response_surface <- function(T,Phe,NaCl) {
   (-0.0639+0.0137*T+0.0103*Phe+0.0257*NaCl-0.0001*T*Phe-0.0018*T*NaCl-0.0011*Phe*NaCl+(0.0004*(T^2))-(0.0002*(Phe^2))-(0.0011*(NaCl^2)))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
