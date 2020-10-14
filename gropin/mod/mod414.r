#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (1.1*(10^-1)*(T-18.5)*((T-3.8)^2))/((12.4-3.8)*((12.4-3.8)*(T-12.4)-(12.4-18.5)*(12.4+3.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
