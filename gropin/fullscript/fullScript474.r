#############################
# start of Parameter script
#############################
T <- seq(6.006,14.985014985015,length.out=21)
Irradiationdose <- seq(1.001,2.4975024975025,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 474 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Irradiationdose)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Irradiationdose) {
   mumax <-5.38-0.94*T+20.2*Irradiationdose+0.04*(T^2)-0.66*(Irradiationdose^2)-2.64*T*Irradiationdose+0.045*T*(Irradiationdose^2)+0.091*Irradiationdose*(T^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Irradiationdose']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 474 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Poultry _Cooked_
(gropin ID:474)'
persp(T,Irradiationdose,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='Irradiationdose',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
