#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- CO2
 
response_surface <- function(T,CO2) {
   (ln(mref)-(dco2*CO2)+(Ea/0.00831)*((1/273)-(1/(T+273))))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
