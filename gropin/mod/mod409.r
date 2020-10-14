#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (3.7*(10^-3)*(T-34.4)*((T+2.9)^2))/((27.2+2.9)*((27.2+2.9)*(T-27.2)-(27.2-34.4)*(27.2-2.9-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
