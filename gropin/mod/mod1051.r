#############################
# start of Model script
#############################
library(hash)
myHash <- hash(pH=pH,TA=TA,Sugar=Sugar,SB=SB,PS=PS)
if (visVar1 == 'pH') {
  multVar1 <- pH
}
if (visVar1 == 'TA') {
  multVar1 <- TA
}
if (visVar1 == 'Sugar') {
  multVar1 <- Sugar
}
if (visVar1 == 'SB') {
  multVar1 <- SB
}
if (visVar1 == 'PS') {
  multVar1 <- PS
}
 
if (visVar2 == 'pH') {
  multVar2 <- pH
}
if (visVar2 == 'TA') {
  multVar2 <- TA
}
if (visVar2 == 'Sugar') {
  multVar2 <- Sugar
}
if (visVar2 == 'SB') {
  multVar2 <- SB
}
if (visVar2 == 'PS') {
  multVar2 <- PS
}
visAxes <- c(visVar1,visVar2)
expectedAxes <- c('pH','TA','Sugar','SB','PS')
notPresent<-match(expectedAxes,visAxes)
if('pH' %in% expectedAxes[is.na(notPresent)]) {myHash['pH'] <- 0}
if('TA' %in% expectedAxes[is.na(notPresent)]) {myHash['TA'] <- 0}
if('Sugar' %in% expectedAxes[is.na(notPresent)]) {myHash['Sugar'] <- 0}
if('SB' %in% expectedAxes[is.na(notPresent)]) {myHash['SB'] <- 0}
if('PS' %in% expectedAxes[is.na(notPresent)]) {myHash['PS'] <- 0}
 
response_surface <- function(pH,TA,Sugar,SB,PS) {
   (-42.43177+24.97906*pH+46.76338*TA-0.77329*Sugar-0.03119*PS-0.00790*SB-2.70679*(pH^2)-14.36986*pH*TA+0.00926*PS*pH+0.00179*PS*Sugar-0.00004*(PS^2)-0.00005*SB*PS+0.00002*(SB^2))
} 
 
notVisibleAxes <- expectedAxes[!expectedAxes %in% visAxes]
result <- outer(as.double(values(myHash[visVar1])),as.double(values(myHash[visVar2])),response_surface,as.double(values(myHash[notVisibleAxes[1]])),as.double(values(myHash[notVisibleAxes[2]])),as.double(values(myHash[notVisibleAxes[3]])))
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
