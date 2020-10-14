#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (9.6*(10^-1)*(T-34.3)*((T+5)^2))/((29.8+5)*((29.8+5)*(T-29.8)-(29.8-34.3)*(29.8-5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
