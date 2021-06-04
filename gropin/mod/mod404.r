############################# 
# start of Model script Gropin ID 404 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(8.9*(10^-1)*(T-35.1)*((T+5)^2))/((31.6+5)*((31.6+5)*(T-31.6)-(31.6-35.1)*(31.6-5-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
