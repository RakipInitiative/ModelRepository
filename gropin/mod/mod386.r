############################# 
# start of Model script Gropin ID 386 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1*(T-46.1)*((T-13)^2))/((37.9-13)*((37.9-13)*(T-37.9)-(37.9-46.1)*(37.9+13-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
