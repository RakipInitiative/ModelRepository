#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.1*(T-46.2)*((T-5)^2))/((38.1-5)*((38.1-5)*(T-38.1)-(38.1-46.2)*(38.1+5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
