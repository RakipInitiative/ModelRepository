#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (7.5*(10^-2)*(T-33.4)*((T+5.2)^2))/((25.5+5.2)*((25.5+5.2)*(T-25.5)-(25.5-33.4)*(25.5-5.2-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
