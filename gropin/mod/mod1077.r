#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- Limonin
 
response_surface <- function(T,Limonin) {
   7.26-0.386*T+0.006*(T^2)-0.257*Limonin+0.008*T*Limonin
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
