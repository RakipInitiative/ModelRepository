#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- pH
 
response_surface <- function(T,pH) {
   sqrt(2.26*((T-68.02)*((T-38.52)^2)/(((57.59-38.52)^(2-1))*((57.59-38.52)*(T-57.59)-(57.59-68.02)*((2-1)*57.59+38.52-2*T))))*((pH-8.91)*((pH-5.27)^1)/(((7.17-5.27)^(0))*((7.17-5.27)*(pH-7.17)-(7.17-8.91)*((0)*7.17+5.27-1*pH)))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
