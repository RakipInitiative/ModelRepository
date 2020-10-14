#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   (a11*(((pH-pHmin)*(pH-pHmax))/((pH-pHsubmin)*(pH-pHsupmax))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
