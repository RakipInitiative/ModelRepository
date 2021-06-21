############################# 
# start of Model script Gropin ID 450 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(CO2) {
   mumax <-0.00204*(31.5-CO2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
