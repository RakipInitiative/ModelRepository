############################# 
# start of Model script Gropin ID 390 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.3*(10^-3)*(T-35.5)*((T+2.8)^2))/((29.3+2.8)*((29.3+2.8)*(T-29.3)-(29.3-35.5)*(29.3-2.8-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
