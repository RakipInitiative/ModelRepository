#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.4*(10^-1)*(T-26.4)*((T+9)^2))/((20.1+9)*((20.1+9)*(T-20.1)-(20.1-26.4)*(20.1-9-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
