#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- notused
 
response_surface <- function(pH,notused) {
   3.28*(pH-9.84)*((pH-3.78)^2)/((6.81-3.78)*((6.81-3.78)*(pH-6.81)-(6.81-9.84)*(6.81+3.78-2*pH)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
