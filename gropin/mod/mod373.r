############################# 
# start of Model script Gropin ID 373 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(8.3*(10^-3)*(T-37.3)*((T-0.1)^2))/((30.2-0.1)*((30.2-0.1)*(T-30.2)-(30.2-37.3)*(30.2+0.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
