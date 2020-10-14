#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (8.7*(10^-3)*(T-44)*((T-5.1)^2))/((35.4-5.1)*((35.4-5.1)*(T-35.4)-(35.4-44)*(35.4+5.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
