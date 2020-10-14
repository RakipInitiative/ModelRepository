#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- S
 
response_surface <- function(pH,S) {
   0.48*(pH-2.3)*(pH-15.1)/((pH-2.3)*(pH-15.1)-(pH-4)^2)*(1-S/981)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
