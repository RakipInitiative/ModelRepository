############################# 
# start of Model script Gropin ID 377 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.6*(T-59.2)*((T-19.8)^2))/((50.3-19.8)*((50.3-19.8)*(T-50.3)-(50.3-59.2)*(50.3+19.8-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
