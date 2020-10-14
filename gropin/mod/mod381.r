#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (5.4*(10^-2)*(T-32)*((T+11.3)^2))/((24.9+11.3)*((24.9+11.3)*(T-24.9)-(24.9-32)*(24.9-11.3-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
