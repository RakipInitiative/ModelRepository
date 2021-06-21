############################# 
# start of Model script Gropin ID 402 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.4*(10^-3)*(T-42.6)*((T-2.1)^2))/((36.7-2.1)*((36.7-2.1)*(T-36.7)-(36.7-42.6)*(36.7+2.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
