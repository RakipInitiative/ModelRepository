#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (8.3*(10^-3)*(T-37.3)*((T-0.1)^2))/((30.2-0.1)*((30.2-0.1)*(T-30.2)-(30.2-37.3)*(30.2+0.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
