#############################
# start of Model script
#############################
library(hash)
myHash <- hash(Fruct=Fruct,NaCl=NaCl,pH=pH,Ac=Ac)
if (visVar1 == 'Fruct') {
  multVar1 <- Fruct
}
if (visVar1 == 'NaCl') {
  multVar1 <- NaCl
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'Ac') {
  multVar1 <- Ac
}
 
if (visVar2 == 'Fruct') {
  multVar2 <- Fruct
}
if (visVar2 == 'NaCl') {
  multVar2 <- NaCl
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'Ac') {
  multVar2 <- Ac
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('Fruct','NaCl','pH','Ac')
notPresent<-match(expectedAxes,visAxes)
if('Fruct' %in% expectedAxes[is.na(notPresent)]) {myHash['Fruct'] <- 0}
if('NaCl' %in% expectedAxes[is.na(notPresent)]) {myHash['NaCl'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('Ac' %in% expectedAxes[is.na(notPresent)]) {myHash['Ac'] <- 0}
 
response_surface <- function(Fruct,NaCl,pH,Ac) {
   4.76+3.79*((Fruct-19.5)/17.7)+1.47*((NaCl-3.4)/1.1)-1.62*((pH-3.8)/0.4)+3.21*((Ac-2.3)/0.7)+0.59*((Fruct-19.5)/17.7)*((NaCl-3.4)/1.1)-0.44*((Fruct-19.5)/17.7)*((pH-3.8)/0.4)+0.82*((Fruct-19.5)/17.7)*((Ac-2.3)/0.7)-0.24*((NaCl-3.4)/1.1)*((pH-3.8)/0.4)+0.46*((NaCl-3.4)/1.1)*((Ac-2.3)/0.7)-0.74*((pH-3.8)/0.4)*((Ac-2.3)/0.7)+0.39*(((Ac-2.3)/0.7)^2)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
