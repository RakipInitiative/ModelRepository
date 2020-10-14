#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (4.3*(10^-3)*(T-35.5)*((T+2.8)^2))/((29.3+2.8)*((29.3+2.8)*(T-29.3)-(29.3-35.5)*(29.3-2.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
