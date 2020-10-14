#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (4.4*(10^-1)*(T-36.8)*((T-1.1)^2))/((30.8-1.1)*((30.8-1.1)*(T-30.8)-(30.8-36.8)*(30.8+1.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
