#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- Irradiationdose
 
response_surface <- function(T,Irradiationdose) {
   5.38-0.94*T+20.2*Irradiationdose+0.04*(T^2)-0.66*(Irradiationdose^2)-2.64*T*Irradiationdose+0.045*T*(Irradiationdose^2)+0.091*Irradiationdose*(T^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
