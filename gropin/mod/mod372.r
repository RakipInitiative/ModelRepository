############################# 
# start of Model script Gropin ID 372 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.3*(10^-3)*(T-35.1)*((T-1.4)^2))/((28.4-1.4)*((28.4-1.4)*(T-28.4)-(28.4-35.1)*(28.4+1.4-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
