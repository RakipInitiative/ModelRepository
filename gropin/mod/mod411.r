#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (4.5*(10^-1)*(T-81.8)*((T-36.8)^2))/((70.9-36.8)*((70.9-36.8)*(T-70.9)-(70.9-81.8)*(70.9+36.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
