#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- PLSDAmix
 
response_surface <- function(T,PLSDAmix) {
   0.1312-0.0322*T+0.0972*PLSDAmix-0.0080*T*PLSDAmix+0.0023*(T^2)-0.0076*(PLSDAmix^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
