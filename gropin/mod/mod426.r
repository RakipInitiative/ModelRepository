#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   EXP(-12.65+0.004234*(T^2)-0.3024*(pH^2)+0.01535*T*pH-0.004356*T+3.467*pH)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
