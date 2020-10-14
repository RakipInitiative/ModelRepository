#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (3.4*(10^-1)*(T-35.2)*((T-0.1)^2))/((28.3-0.1)*((28.3-0.1)*(T-28.3)-(28.3-35.2)*(28.3+0.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
