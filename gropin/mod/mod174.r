#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   ((-b13)*((T-Tmin)^2)*(pH-pHmin)*(pH-pHmax))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
