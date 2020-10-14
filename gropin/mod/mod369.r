#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (6.6*(10^-1)*(T-31.5)*((T+10)^2))/((26.2+10)*((26.2+10)*(T-26.2)-(26.2-31.5)*(26.2-10-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
