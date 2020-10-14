#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- S
 
response_surface <- function(pH,S) {
   (-1.4*(1-1.9/pH)*(1-EXP(1-pH/10.4))*(1-S/980))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
