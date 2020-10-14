#############################
# start of Model script
#############################
multVar1 <- Sugar
multVar2 <- pH
 
response_surface <- function(Sugar,pH) {
   4655.09141-141.06579*Sugar+1.23229*(Sugar^2)-89.94556*pH+71.96275*(pH^2)-5.50379*Sugar*pH
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
