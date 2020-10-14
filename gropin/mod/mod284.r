#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.35*((T-8.44)^2)*(T-46.4))/((41.5-8.44)*((41.5-8.44)*(T-41.5)-(41.5-46.4)*(8.44+41.5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
