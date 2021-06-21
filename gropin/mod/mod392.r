############################# 
# start of Model script Gropin ID 392 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.4*(10^-1)*(T-39.7)*((T-2.3)^2))/((30.8-2.3)*((30.8-2.3)*(T-30.8)-(30.8-39.7)*(30.8+2.3-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
