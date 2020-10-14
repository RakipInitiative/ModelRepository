#############################
# start of Model script
#############################
multVar1 <- O2
multVar2 <- notused
 
response_surface <- function(O2,notused) {
   0.674*(1-(O2/3.26)^2.983)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
