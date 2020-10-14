#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (3*(T-71.8)*((T-33.6)^2))/((64.8-33.6)*((64.8-33.6)*(T-64.8)-(64.8-71.8)*(64.8+33.6-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
