############################# 
# start of Model script Gropin ID 396 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3.4*(10^-1)*(T-35.2)*((T-0.1)^2))/((28.3-0.1)*((28.3-0.1)*(T-28.3)-(28.3-35.2)*(28.3+0.1-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
