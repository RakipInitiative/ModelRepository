#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   (-3.929+0.185*T+0.2142*pH-0.003181*(T^2)-0.008647*(pH^2)-0.001372*T*pH)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
