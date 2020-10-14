#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   0.975*((T-45.7)*((T-1.9)^2))/((36.4*(T-38.3)-(-7.4)*(36.4-2*T))*36.4)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
