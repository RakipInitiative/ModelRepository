#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- aw
 
response_surface <- function(pH,aw) {
   (-1892+81.006*pH+3270*aw+(-79.839)*pH*aw+(-2.3715)*(pH^2)+(-1382.3)*(aw^2))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
