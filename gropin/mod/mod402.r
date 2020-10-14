#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (6.4*(10^-3)*(T-42.6)*((T-2.1)^2))/((36.7-2.1)*((36.7-2.1)*(T-36.7)-(36.7-42.6)*(36.7+2.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
