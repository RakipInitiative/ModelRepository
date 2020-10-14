#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (5.1*(10^-1)*(T-36.3)*((T+1.8)^2))/((27.8+1.8)*((27.8+1.8)*(T-27.8)-(27.8-36.3)*(27.8-1.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
