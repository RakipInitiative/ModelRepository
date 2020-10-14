#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.4*(T-47.3)*((T-5.6)^2))/((40.3-5.6)*((40.3-5.6)*(T-40.3)-(40.3-47.3)*(40.3+5.6-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
