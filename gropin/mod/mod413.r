#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (5.1*(10^-1)*(T-29.7)*((T-6.5)^2))/((23.8-6.5)*((23.8-6.5)*(T-23.8)-(23.8-29.7)*(23.8+6.5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
