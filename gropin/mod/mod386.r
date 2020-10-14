#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1*(T-46.1)*((T-13)^2))/((37.9-13)*((37.9-13)*(T-37.9)-(37.9-46.1)*(37.9+13-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
