#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- Irradiationdose
 
response_surface <- function(T,Irradiationdose) {
   (-0.899+0.252*T+0.374*Irradiationdose-0.0045*(T^2)-0.05*(Irradiationdose^2)-0.085*T*Irradiationdose+0.01*T*(Irradiationdose^2)+0.0024*Irradiationdose*(T^2))
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
