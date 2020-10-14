#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- aw
 
response_surface <- function(pH,aw) {
   (-112.09+(-27.702)*pH+467.55*aw+6.9724*pH*aw+1.5135*(pH^2)+(-282.75)*(aw^2))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
