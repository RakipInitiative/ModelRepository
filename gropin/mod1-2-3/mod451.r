############################# 
# start of Model script Gropin ID 451 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(CO2) {
   mumax <-sqrt(118*(CO2+0.007))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
