#############################
# start of Model script
#############################
multVar1 <- O2
multVar2 <- notused
 
response_surface <- function(O2,notused) {
   0.632*O2/(0.118+O2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
