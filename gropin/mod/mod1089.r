#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- Sugar
 
response_surface <- function(pH,Sugar) {
   2849.592-79.148*Sugar+0.596*(Sugar^2)-232.069*pH+20.771*(pH^2)+1.537*Sugar*pH
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
