#############################
# start of Model script
#############################
multVar1 <- T
multVar2 <- Irradiationdose
 
response_surface <- function(T,Irradiationdose) {
   10.05-1.81*T+19.86*Irradiationdose+0.078*(T^2)-2.46*(Irradiationdose^2)-2.24*T*Irradiationdose+0.15*T*(Irradiationdose^2)+0.068*Irradiationdose*(T^2)
} 
result <- outer(multVar1,multVar2,response_surface)
colnames(result)<-multVar2
rownames(result)<-multVar1
#############################
# End of Model script
#############################
