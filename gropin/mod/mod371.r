#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (5.9*(10^-1)*(T-32.4)*((T+10.5)^2))/((25.3+10.5)*((25.3+10.5)*(T-25.3)-(25.3-32.4)*(25.3-10.5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
