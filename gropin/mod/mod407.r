#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (4.3*(10^-3)*(T-34.4)*((T+0.5)^2))/((30+0.5)*((30+0.5)*(T-30)-(30-34.4)*(30-0.5-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
