############################# 
# start of Model script Gropin ID 438 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2,NaCl,NaNO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(CO2,NaCl,NaNO2) {
   mumax <-5.6+50*CO2*NaNO2+21*(NaCl^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['CO2'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
