#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (8*(10^-1)*(T-34)*((T+10.1)^2))/((30.8+10.1)*((30.8+10.1)*(T-30.8)-(30.8-34)*(30.8-10.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
