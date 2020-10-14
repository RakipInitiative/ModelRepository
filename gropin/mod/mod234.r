#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- notused
 
response_surface <- function(pH,notused) {
   mopt*(((pH-pHmin)*(1-EXP(c2*(pH-pHmax))))/((pHopt-pHmin)*(1-EXP(c2*(pHopt-pHmax)))))^2
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
