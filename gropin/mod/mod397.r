############################# 
# start of Model script Gropin ID 397 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.3*(10^-3)*(T-37.2)*((T+0.9)^2))/((30.5+0.9)*((30.5+0.9)*(T-30.5)-(30.5-37.2)*(30.5-0.9-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
