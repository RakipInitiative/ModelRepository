#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   0.0571-0.0091*pH+0.0005*(pH^2)-0.0070*T+0.0006*(T^2)+0.0006*(pH*T)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
