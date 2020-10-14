#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- days
 
response_surface <- function(T,days) {
   0.247+0.297*days+0.363*T-0.0075*(days^2)-0.00832*(T^2)-0.000796*T*days
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
