#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.1*(T-45.8)*((T-11)^2))/((39.3-11)*((39.3-11)*(T-39.3)-(39.3-45.8)*(39.3+11-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
