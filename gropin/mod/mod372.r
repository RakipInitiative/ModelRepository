#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (6.3*(10^-3)*(T-35.1)*((T-1.4)^2))/((28.4-1.4)*((28.4-1.4)*(T-28.4)-(28.4-35.1)*(28.4+1.4-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
