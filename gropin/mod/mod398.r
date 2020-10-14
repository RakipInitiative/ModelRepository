#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (4.3*(10^-1)*(T-30.7)*((T+5.7)^2))/((28.4+5.7)*((28.4+5.7)*(T-28.4)-(28.4-30.7)*(28.4-5.7-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
