#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- Limonin
 
response_surface <- function(T,Limonin) {
   (-18.12+1.575*T-0.023*T*Limonin)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
