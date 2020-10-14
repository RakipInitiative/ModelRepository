#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   (-0.417+(30.548)* sqrt(1-aw)-92.980*( sqrt(1-aw)* sqrt(1-aw))+0.242*T+0.0018*T*T-0.849* sqrt(1-aw)*T)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
