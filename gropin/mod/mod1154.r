#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   EXP(2.21)*(0.633*(T-44.3)*((T-11.5)^2)/(((36.4-11.5)*(T-36.4)-(36.4-44.3)*(36.4+11.5-2*T))*(36.4-11.5)))^(-0.786)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
