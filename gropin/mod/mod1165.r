############################# 
# start of Model script Gropin ID 1165 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.195*(T-47.97)*((T-6.24)^2))/(((38.37-6.24)*(T-38.37)-(38.37-47.97)*(38.37+6.24-2*T))*(38.37-6.24))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
