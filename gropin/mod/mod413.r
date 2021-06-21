############################# 
# start of Model script Gropin ID 413 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.1*(10^-1)*(T-29.7)*((T-6.5)^2))/((23.8-6.5)*((23.8-6.5)*(T-23.8)-(23.8-29.7)*(23.8+6.5-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
