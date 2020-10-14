#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.5*(10^-1)*(T-26.8)*((T+9.7)^2))/((21.8+9.7)*((21.8+9.7)*(T-21.8)-(21.8-26.8)*(21.8-9.7-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
