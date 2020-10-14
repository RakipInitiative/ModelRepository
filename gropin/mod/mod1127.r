#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- PLSDAmix
 
response_surface <- function(T,PLSDAmix) {
   (-0.1831)+0.0172*T-0.0231*PLSDAmix-0.0046*T*PLSDAmix+0.0009*(T^2)+0.0126*(PLSDAmix^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
