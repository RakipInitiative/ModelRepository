#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- Irradiationdose
 
response_surface <- function(T,Irradiationdose) {
   0.42-0.036*T+0.18*Irradiationdose+0.01*(T^2)-0.065*(Irradiationdose^2)-0.054*T*Irradiationdose+0.014*T*(Irradiationdose^2)-0.000088*Irradiationdose*(T^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
