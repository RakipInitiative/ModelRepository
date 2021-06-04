############################# 
# start of Model script Gropin ID 379 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3*(T-71.8)*((T-33.6)^2))/((64.8-33.6)*((64.8-33.6)*(T-64.8)-(64.8-71.8)*(64.8+33.6-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
