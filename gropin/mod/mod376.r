#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1*(T-62.9)*((T-12.1)^2))/((53.8-12.1)*((53.8-12.1)*(T-53.8)-(53.8-62.9)*(53.8+12.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
