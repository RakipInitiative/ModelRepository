#############################
# start of Model script
#############################
multVar1 <- CO2
multVar2 <- notused
 
response_surface <- function(CO2,notused) {
   0.00204*(31.5-CO2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
