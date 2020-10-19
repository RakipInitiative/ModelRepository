#############################
# start of Model script
#############################
multVar1 <- aw
multVar2 <- notused
 
response_surface <- function(aw,notused) {
   0.213+0.939*sqrt(1-aw)+6.015*((sqrt(1-aw))^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################