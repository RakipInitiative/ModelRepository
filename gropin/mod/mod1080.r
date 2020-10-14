#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- Limonin
 
response_surface <- function(T,Limonin) {
   (-191+ 14.04*T-0.227*(T^2)-0.029*(Limonin^2))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
