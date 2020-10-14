#############################
# start of Model script
#############################
library(hash)
myHash <- hash(T=T,pH=pH,aw=aw,Phe=Phe,NaNO2=NaNO2,CO2=CO2,UAc=UAc,ULa=ULa,UDiac=UDiac)
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'Phe') {
  multVar1 <- Phe
}
if (visVar1 == 'NaNO2') {
  multVar1 <- NaNO2
}
if (visVar1 == 'CO2') {
  multVar1 <- CO2
}
if (visVar1 == 'UAc') {
  multVar1 <- UAc
}
if (visVar1 == 'ULa') {
  multVar1 <- ULa
}
if (visVar1 == 'UDiac') {
  multVar1 <- UDiac
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
if (visVar2 == 'Phe') {
  multVar2 <- Phe
}
if (visVar2 == 'NaNO2') {
  multVar2 <- NaNO2
}
if (visVar2 == 'CO2') {
  multVar2 <- CO2
}
if (visVar2 == 'UAc') {
  multVar2 <- UAc
}
if (visVar2 == 'ULa') {
  multVar2 <- ULa
}
if (visVar2 == 'UDiac') {
  multVar2 <- UDiac
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','pH','aw','Phe','NaNO2','CO2','UAc','ULa','UDiac')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {myHash['T'] <- 0}
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {myHash['aw'] <- 0}
if('Phe' %in% expectedAxes[is.na(notPresent)]) {myHash['Phe'] <- 0}
if('NaNO2' %in% expectedAxes[is.na(notPresent)]) {myHash['NaNO2'] <- 0}
if('CO2' %in% expectedAxes[is.na(notPresent)]) {myHash['CO2'] <- 0}
if('UAc' %in% expectedAxes[is.na(notPresent)]) {myHash['UAc'] <- 0}
if('ULa' %in% expectedAxes[is.na(notPresent)]) {myHash['ULa'] <- 0}
if('UDiac' %in% expectedAxes[is.na(notPresent)]) {myHash['UDiac'] <- 0}
 
response_surface <- function(T,pH,aw,Phe,NaNO2,CO2,UAc,ULa,UDiac) {
   0.419*(((T-(-2.83))^2)/((25-(-2.83))^2))*((aw-0.923)/(1-0.923))*(1-(10^(4.97-pH)))*(1-(ULa/3.79))*((32-Phe)/32)*(((350-NaNO2)/350)^2)*((3140-CO2)/3140)*(1-sqrt(UDiac/4.8))*(1-sqrt(UAc/10.3))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])),as.double(values(myHash[notVisibleAxes[3]])),as.double(values(myHash[notVisibleAxes[4]])),as.double(values(myHash[notVisibleAxes[5]])),as.double(values(myHash[notVisibleAxes[6]])),as.double(values(myHash[notVisibleAxes[7]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
