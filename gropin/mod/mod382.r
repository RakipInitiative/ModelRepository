#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (6.2*(10^-2)*(T-33.1)*((T+7.5)^2))/((27.8+7.5)*((27.8+7.5)*(T-27.8)-(27.8-33.1)*(27.8-7.5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
