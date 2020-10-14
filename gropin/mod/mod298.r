#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,aw=aw,NaNO2=NaNO2)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'NaNO2') {
  multVar2 <- NaNO2
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','aw','NaNO2')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
 
response_surface <- function(T,pH,aw,NaNO2) {
   LN(2)/EXP(21.2574-0.2643*T+0.00404*(-33.3333+0.0220472*((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3))-15712.5/((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3)))-5.2054*pH+0.0189*NaNO2+0.00709*T*pH-0.00252*pH*NaNO2+0.00265*(T^2)+0.000129*((-33.3333+0.0220472*((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3))-15712.5/((sqrt(1.51165*(10^21)*(aw^2)-3.28336*(10^21)*aw+1.78325*(10^21))-3.888*(10^10)*aw+4.22242*(10^10))^(1/3)))^2)+0.3746*(pH^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
