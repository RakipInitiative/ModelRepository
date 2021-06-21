############################# 
# start of Model script Gropin ID 409 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3.7*(10^-3)*(T-34.4)*((T+2.9)^2))/((27.2+2.9)*((27.2+2.9)*(T-27.2)-(27.2-34.4)*(27.2-2.9-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
