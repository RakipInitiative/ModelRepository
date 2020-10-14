#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- days
 
response_surface <- function(T,days) {
   0.89+0.081*days+0.11*T+0.000192*(days^2)-0.0034*(T^2)+0.0068*T*days
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
