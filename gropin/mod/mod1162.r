#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- notused
 
response_surface <- function(T,notused) {
   ((sqrt(0.047^2)*sqrt((T-10.69)^2)*sqrt((sqrt(1-EXP(0.252*(T-52.8))))^2))^(-1))*LN(1+(1/0.0137))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
