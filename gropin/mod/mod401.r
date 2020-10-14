#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.2*(T-45.4)*((T-9.8)^2))/((38.6-9.8)*((38.6-9.8)*(T-38.6)-(38.6-45.4)*(38.6+9.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
