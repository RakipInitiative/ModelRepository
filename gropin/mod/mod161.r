#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,CitrA=CitrA,AscorbA=AscorbA)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'CitrA') {
  multVar1 <- CitrA
}
if (visVar1 == 'AscorbA') {
  multVar1 <- AscorbA
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'CitrA') {
  multVar2 <- CitrA
}
if (visVar2 == 'AscorbA') {
  multVar2 <- AscorbA
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','CitrA','AscorbA')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('CitrA' %in% expectedAxes[is.na(notPresent)]) {myHash['CitrA'] <- 0}
if('AscorbA' %in% expectedAxes[is.na(notPresent)]) {myHash['AscorbA'] <- 0}
 
response_surface <- function(T,CitrA,AscorbA) {
   (0.1873+(0.7077*T)-(0.4681*CitrA)-(0.0706*AscorbA)+(0.03353*(T^2))+(0.5058*(CitrA^2))-(1.1107*T*CitrA)-(0.4981*T*AscorbA)+(0.3896*CitrA*AscorbA))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
