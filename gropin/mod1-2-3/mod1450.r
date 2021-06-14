############################# 
# start of Model script Gropin ID 1450 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.7320*((T-49.26)*(T-5.699)^2)/((40.01-5.699)*((40.01-5.699)*(T-40.01)-(40.01-49.26)*(40.01+5.699-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
