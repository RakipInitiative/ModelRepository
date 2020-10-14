#############################
# start of Model script
#############################
multVar1 <- Sugar
multVar2 <- pH
 
response_surface <- function(Sugar,pH) {
   12.63699-0.50885*Sugar+0.00459363*(Sugar^2)+4.26298*pH-0.038048*(pH^2)-0.05637*Sugar*pH
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
