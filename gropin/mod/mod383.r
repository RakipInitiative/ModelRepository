############################# 
# start of Model script Gropin ID 383 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.5*(10^-1)*(T-26.8)*((T+9.7)^2))/((21.8+9.7)*((21.8+9.7)*(T-21.8)-(21.8-26.8)*(21.8-9.7-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
