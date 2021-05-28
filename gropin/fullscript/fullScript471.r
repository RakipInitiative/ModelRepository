#############################
# start of Parameter script
#############################
T <- seq(5.99400599400599,15.015,length.out=21)
Irradiationdose <- seq(0.999000999000999,2.5025,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 471 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Irradiationdose)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,Irradiationdose) {
   mumax <-(-0.899+0.252*T+0.374*Irradiationdose-0.0045*(T^2)-0.05*(Irradiationdose^2)-0.085*T*Irradiationdose+0.01*T*(Irradiationdose^2)+0.0024*Irradiationdose*(T^2))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Irradiationdose']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 471 
#############################
persp(T,Irradiationdose,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='Irradiationdose',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Poultry _Raw_
(gropin ID:471)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
