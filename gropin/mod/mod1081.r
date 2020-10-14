#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,Time=Time,TA=TA,SS=SS)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'Time') {
  multVar1 <- Time
}
if (visVar1 == 'TA') {
  multVar1 <- TA
}
if (visVar1 == 'SS') {
  multVar1 <- SS
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'Time') {
  multVar2 <- Time
}
if (visVar2 == 'TA') {
  multVar2 <- TA
}
if (visVar2 == 'SS') {
  multVar2 <- SS
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','Time','TA','SS')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('Time' %in% expectedAxes[is.na(notPresent)]) {myHash['Time'] <- 0}
if('TA' %in% expectedAxes[is.na(notPresent)]) {myHash['TA'] <- 0}
if('SS' %in% expectedAxes[is.na(notPresent)]) {myHash['SS'] <- 0}
 
response_surface <- function(T,Time,TA,SS) {
   (-14.54+(0.64*T)+0.40*Time-0.21*TA-0.02*SS)
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
