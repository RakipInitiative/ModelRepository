############################# 
# start of Model script Gropin ID 439 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(CO2,NaCl,NaNO2) {
   mumax <-1-0.4*NaNO2+0.3*CO2*NaNO2-0.5*(NaCl^2)-0.4*(CO2^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['CO2'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
