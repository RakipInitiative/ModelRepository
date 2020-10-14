#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.8*(10^-2)*(T-65.5)*((T-30.8)^2))/((57.2-30.8)*((57.2-30.8)*(T-57.2)-(57.2-65.5)*(57.2+30.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
