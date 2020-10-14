#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- Sugar
 
response_surface <- function(pH,Sugar) {
   (-2221.43+85.390*Sugar-0.700*(Sugar^2)-419.81*pH+45.270*(pH^2)+2.45*Sugar*pH)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
