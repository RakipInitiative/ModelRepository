############################# 
# start of Model script Gropin ID 415 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.4*(10^-1)*(T-36.8)*((T-1.1)^2))/((30.8-1.1)*((30.8-1.1)*(T-30.8)-(30.8-36.8)*(30.8+1.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
