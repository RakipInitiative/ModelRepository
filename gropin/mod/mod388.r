#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.1*(T-48)*((T-11.2)^2))/((41-11.2)*((41-11.2)*(T-41)-(41-48)*(41+11.2-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
