#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   EXP(-919.54+1.6033*((10^5)/(T+273))-2.3784*((10^7)/((T+273)^2))+1317.7*aw-669*(aw^2))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
