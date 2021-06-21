############################# 
# start of Model script Gropin ID 473 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Irradiationdose)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Irradiationdose) {
   mumax <-10.05-1.81*T+19.86*Irradiationdose+0.078*(T^2)-2.46*(Irradiationdose^2)-2.24*T*Irradiationdose+0.15*T*(Irradiationdose^2)+0.068*Irradiationdose*(T^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Irradiationdose']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
