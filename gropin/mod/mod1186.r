#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   (-0.150*(T-0.173))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################