#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   2.21*(T-46.8)*((T-8.23)^2)/(((37.6-8.23)*(T-37.6)-(37.6-46.8)*(37.6+8.23-2*T))*(37.6-8.23))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
