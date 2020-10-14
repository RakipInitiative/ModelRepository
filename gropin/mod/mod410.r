#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.3*(T-31.3)*((T+2.1)^2))/((25.8+2.1)*((25.8+2.1)*(T-25.8)-(25.8-31.3)*(25.8-2.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
