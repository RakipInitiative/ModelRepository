#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (3.8*(10^-1)*(T-37.9)*((T-0.9)^2))/((30.9-0.9)*((30.9-0.9)*(T-30.9)-(30.9-37.9)*(30.9+0.9-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
