#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (3*(10^-1)*(T-29)*((T+2.9)^2))/((25.4+2.9)*((25.4+2.9)*(T-25.4)-(25.4-29)*(25.4-2.9-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
