#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.4*(10^-1)*(T-39.8)*((T-1.5)^2))/((30.9-1.5)*((30.9-1.5)*(T-30.9)-(30.9-39.8)*(30.9+1.5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
