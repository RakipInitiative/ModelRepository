############################# 
# start of Model script Gropin ID 400 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.7*(T-46)*((T-4.4)^2))/((40.3-4.4)*((40.3-4.4)*(T-40.3)-(40.3-46)*(40.3+4.4-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
