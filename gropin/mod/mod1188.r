#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   (-0.125+(22.690)* sqrt(1-aw)-71.360*( sqrt(1-aw)* sqrt(1-aw))+0.267*T+0.0015*T*T-0.877* sqrt(1-aw)*T)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
