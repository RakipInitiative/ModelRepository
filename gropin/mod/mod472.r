############################# 
# start of Model script Gropin ID 472 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Irradiationdose)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Irradiationdose) {
   mumax <-0.42-0.036*T+0.18*Irradiationdose+0.01*(T^2)-0.065*(Irradiationdose^2)-0.054*T*Irradiationdose+0.014*T*(Irradiationdose^2)-0.000088*Irradiationdose*(T^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Irradiationdose']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
