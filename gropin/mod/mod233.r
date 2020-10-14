#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- notused
 
response_surface <- function(pH,notused) {
   b2*((pH-pHmin)*(1-EXP(c1*(pH-pHmax))))^2
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
