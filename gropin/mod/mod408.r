#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (4.7*(10^-3)*(T-29.4)*((T+3.5)^2))/((26+3.5)*((26+3.5)*(T-26)-(26-29.4)*(26-3.5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
