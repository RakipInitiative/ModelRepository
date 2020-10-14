#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.2*(T-51)*((T-13.4)^2))/((38.7-13.4)*((38.7-13.4)*(T-38.7)-(38.7-51)*(38.7+13.4-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
