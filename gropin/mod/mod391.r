############################# 
# start of Model script Gropin ID 391 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.4*(10^-1)*(T-26.4)*((T+9)^2))/((20.1+9)*((20.1+9)*(T-20.1)-(20.1-26.4)*(20.1-9-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
