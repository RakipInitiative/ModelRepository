#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   0.027*(T-5.42)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
