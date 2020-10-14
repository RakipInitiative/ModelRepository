#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- aw
 
response_surface <- function(pH,aw) {
   (-19.684+0.5085*pH+36.254*aw-0.4970*pH*aw+0.0046939*(pH^2)-16.581*(aw^2))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
