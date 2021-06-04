############################# 
# start of Model script Gropin ID 375 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(7.5*(10^-2)*(T-33.4)*((T+5.2)^2))/((25.5+5.2)*((25.5+5.2)*(T-25.5)-(25.5-33.4)*(25.5-5.2-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
