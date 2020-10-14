#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (0.911*(T-53.69)*((T-7.90)^2))/(((37.56-7.90)*(T-37.56)-(37.56-53.69)*(37.56+7.90-2*T))*(37.56-7.90))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
