############################# 
# start of Model script Gropin ID 414 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.1*(10^-1)*(T-18.5)*((T-3.8)^2))/((12.4-3.8)*((12.4-3.8)*(T-12.4)-(12.4-18.5)*(12.4+3.8-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
