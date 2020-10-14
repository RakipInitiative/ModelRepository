#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.195*(T-47.97)*((T-6.24)^2))/(((38.37-6.24)*(T-38.37)-(38.37-47.97)*(38.37+6.24-2*T))*(38.37-6.24))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
