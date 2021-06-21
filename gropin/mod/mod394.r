############################# 
# start of Model script Gropin ID 394 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.9*(10^-1)*(T-25.5)*((T+7.1)^2))/((21.5+7.1)*((21.5+7.1)*(T-21.5)-(21.5-25.5)*(21.5-7.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
