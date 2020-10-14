#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.6*(T-59.2)*((T-19.8)^2))/((50.3-19.8)*((50.3-19.8)*(T-50.3)-(50.3-59.2)*(50.3+19.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
