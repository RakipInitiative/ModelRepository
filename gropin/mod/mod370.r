#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (5.8*(10^-1)*(T-30)*((T+7.8)^2))/((24.6+7.8)*((24.6+7.8)*(T-24.6)-(24.6-30)*(24.6-7.8-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
