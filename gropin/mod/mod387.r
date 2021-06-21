############################# 
# start of Model script Gropin ID 387 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.4*(T-47.3)*((T-5.6)^2))/((40.3-5.6)*((40.3-5.6)*(T-40.3)-(40.3-47.3)*(40.3+5.6-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
