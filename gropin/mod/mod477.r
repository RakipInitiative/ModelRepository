#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- aw
 
response_surface <- function(pH,aw) {
   498.85+(-18.201)*pH+(-876.72)*aw+17.984*pH*aw+0.19199*(pH^2)+381.26*(aw^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
