#############################
# start of Model script
#############################
multVar1 <- CO2
multVar2 <- notused
 
response_surface <- function(CO2,notused) {
   sqrt(118*(CO2+0.007))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
