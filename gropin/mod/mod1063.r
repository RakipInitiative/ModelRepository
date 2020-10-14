#############################
# start of Model script
#############################
multVar1 <- Sugar
multVar2 <- pH
 
response_surface <- function(Sugar,pH) {
   4993.00351-147.44162*Sugar+1.25367*(Sugar^2)-161.13055*pH+71.38168*(pH^2)-4.43598*Sugar*pH
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
