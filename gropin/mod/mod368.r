#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.3*(T-47.5)*((T-4.9)^2))/((41.3-4.9)*((41.3-4.9)*(T-41.3)-(41.3-47.5)*(41.3+4.9-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
