#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- PLSDAmix
 
response_surface <- function(T,PLSDAmix) {
   7.7668-0.4074*T+0.5026*PLSDAmix+0.0016*T*PLSDAmix+0.0063*(T^2)-0.0601*(PLSDAmix^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
