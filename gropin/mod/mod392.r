#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.4*(10^-1)*(T-39.7)*((T-2.3)^2))/((30.8-2.3)*((30.8-2.3)*(T-30.8)-(30.8-39.7)*(30.8+2.3-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
