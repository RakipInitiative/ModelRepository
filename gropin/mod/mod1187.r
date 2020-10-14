#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   (2.053+(-6.484)* sqrt(1-aw)+1.210*( sqrt(1-aw)* sqrt(1-aw))+0.248*T+0.0015*T*T-0.917* sqrt(1-aw)*T)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
