#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   2.068*(((T-68.14)*((T-33.76)^2))/((61.82-33.76)*((61.82-33.76)*(T-61.82)-(61.82-68.14)*(61.82+33.76-2*T))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
