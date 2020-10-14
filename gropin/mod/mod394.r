#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (2.9*(10^-1)*(T-25.5)*((T+7.1)^2))/((21.5+7.1)*((21.5+7.1)*(T-21.5)-(21.5-25.5)*(21.5-7.1-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
