#############################
# start of Model script
#############################
multVar1 <- O2
multVar2 <- notused
 
response_surface <- function(O2,notused) {
   0.368*(1-(O2/6.61)^0.764)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
