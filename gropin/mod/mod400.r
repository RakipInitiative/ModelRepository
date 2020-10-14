#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.7*(T-46)*((T-4.4)^2))/((40.3-4.4)*((40.3-4.4)*(T-40.3)-(40.3-46)*(40.3+4.4-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
