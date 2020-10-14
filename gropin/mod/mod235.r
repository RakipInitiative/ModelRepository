#############################
# start of Model script
#############################
multVar1 <- pH
multVar2 <- T
 
response_surface <- function(pH,T) {
   2.635*(((pH-pHmin)*(pH-pHmax)/(((pH-pHmin)*(pH-pHmax))-(pH-pHopt)^2)))*((T-Tmax)*(T-Tmin)^2)/((Topt-Tmin)*((Topt-Tmin)*(T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T)))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
