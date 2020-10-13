#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988,length.out=21)
aw <- seq(0.9892883,0.9612378,length.out=21)
CO2_dissolved_ <- seq(0,1984.014,length.out=21)
NaL <- seq(0,2.997,length.out=21)
b <- 0.000578
awmin <- 0.9544
Tmin <- -9.0299999999999994
CO2max <- 6691
NaLmax <- 5.87
visVar1 <- 'T'
visVar2 <- 'aw'
#############################
# end of Parameter script
#############################
 
#############################
# start of Model script
#############################
if (visVar1 == 'T') {
  multVar1 <- T
}
if (visVar1 == 'aw') {
  multVar1 <- aw
}
if (visVar1 == 'CO2_dissolved_') {
  multVar1 <- CO2_dissolved_
}
if (visVar1 == 'NaL') {
  multVar1 <- NaL
}
 
if (visVar2 == 'T') {
  multVar2 <- T
}
if (visVar2 == 'aw') {
  multVar2 <- aw
}
if (visVar2 == 'CO2_dissolved_') {
  multVar2 <- CO2_dissolved_
}
if (visVar2 == 'NaL') {
  multVar2 <- NaL
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('T','aw','CO2_dissolved_','NaL')
notPresent<-match(expectedAxes,visAxes)
if('T' %in% expectedAxes[is.na(notPresent)]) {T <- 0}
if('aw' %in% expectedAxes[is.na(notPresent)]) {aw <- 0}
if('CO2_dissolved_' %in% expectedAxes[is.na(notPresent)]) {CO2_dissolved_ <- 0}
if('NaL' %in% expectedAxes[is.na(notPresent)]) {NaL <- 0}
 
response_surface <- function(T,aw,CO2_dissolved_,NaL) {
   (b*(T-Tmin)*sqrt((aw-awmin)*(CO2max-CO2_dissolved_)*(NaLmax-NaL)))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(multVar1,multVar2,response_surface,notVisibleAxes[1],notVisibleAxes[2])
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
 
#############################
# start of Visualisation script
#############################
persp(multVar1,multVar2,result,col = 'green',xlab=visVar1,ylab=visVar2,zlab='mu_max',theta=35,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
