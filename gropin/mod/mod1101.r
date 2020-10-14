#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   sqrt((1/10.3)* ((T-64.2)*((T-38.2)^2))/((53.6-38.2)* (((53.6-38.2)*(T-53.6)-(53.6-64.2)*(53.6+38.2-2*T)))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
