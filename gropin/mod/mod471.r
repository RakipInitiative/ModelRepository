############################# 
# start of Model script Gropin ID 471 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Irradiationdose)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Irradiationdose) {
   mumax <-(-0.899+0.252*T+0.374*Irradiationdose-0.0045*(T^2)-0.05*(Irradiationdose^2)-0.085*T*Irradiationdose+0.01*T*(Irradiationdose^2)+0.0024*Irradiationdose*(T^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Irradiationdose']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
