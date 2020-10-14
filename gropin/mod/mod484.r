#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   0.3228-0.147*(pH)+0.0179*(pH^2)+0.0046*(T)+0.0007*(T^2)-0.0017*(pH*T)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
