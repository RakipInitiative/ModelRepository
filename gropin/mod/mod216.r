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
   (61.354-11.657*T+10.159*Phe+14.752*NaCl-0.147*(T*Phe)-0.863*(T*NaCl)+0.423*(Phe*NaCl)+0.321*(T^2)-0.231*(Phe^2)+0.485*(NaCl^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
