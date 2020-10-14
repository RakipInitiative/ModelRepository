#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- Sugar
 
response_surface <- function(pH,Sugar) {
   2771.561-75.864*Sugar+0.573*(Sugar^2)-228.12*pH+27.544*(pH^2)+0.956*Sugar*pH
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
