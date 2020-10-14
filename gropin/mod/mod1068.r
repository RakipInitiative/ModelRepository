#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   17.507-(0.836*T)+(1.531*pH)+(0.130*T*pH)+(0.002*T^2)-(0.753*pH^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
