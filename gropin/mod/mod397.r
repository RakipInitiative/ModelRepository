#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (6.3*(10^-3)*(T-37.2)*((T+0.9)^2))/((30.5+0.9)*((30.5+0.9)*(T-30.5)-(30.5-37.2)*(30.5-0.9-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
