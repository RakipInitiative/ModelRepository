#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- aw
 
response_surface <- function(T,aw) {
   0.03346*(T+7.6)*sqrt(aw-0.947)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
