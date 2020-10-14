#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- Sugar
 
response_surface <- function(pH,Sugar) {
   (-50.035+1.409*Sugar-0.010*(Sugar^2)+3.765*pH-0.162*(pH^2)-0.041*Sugar*pH)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
