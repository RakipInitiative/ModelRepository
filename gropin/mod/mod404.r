#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (8.9*(10^-1)*(T-35.1)*((T+5)^2))/((31.6+5)*((31.6+5)*(T-31.6)-(31.6-35.1)*(31.6-5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
