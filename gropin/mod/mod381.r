############################# 
# start of Model script Gropin ID 381 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.4*(10^-2)*(T-32)*((T+11.3)^2))/((24.9+11.3)*((24.9+11.3)*(T-24.9)-(24.9-32)*(24.9-11.3-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
