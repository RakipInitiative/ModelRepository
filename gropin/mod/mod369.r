############################# 
# start of Model script Gropin ID 369 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.6*(10^-1)*(T-31.5)*((T+10)^2))/((26.2+10)*((26.2+10)*(T-26.2)-(26.2-31.5)*(26.2-10-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
