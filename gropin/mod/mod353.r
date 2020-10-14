#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (-0.366+3.003*EXP(-((LN(2)/(LN(0.296)^2))*LN((((T-42.330)*((0.296^2)-1))/(15.653*0.296))+1)^2)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
