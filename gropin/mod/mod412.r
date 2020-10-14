#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.1*(10^-1)*(T-19.5)*((T-2.7)^2))/((15.6-2.7)*((15.6-2.7)*(T-15.6)-(15.6-19.5)*(15.6+2.7-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
