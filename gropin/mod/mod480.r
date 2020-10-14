#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- NO2
 
response_surface <- function(pH,NO2) {
   (-0.12635+0.027170*pH+0.031746*NO2+(-0.008778)*pH*NO2+(-0.005576)*(pH^2)+0.024220*(NO2^2))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
