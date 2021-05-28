############################# 
# start of Model script Gropin ID 389 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.3*(T-47.5)*((T-4.9)^2))/((41.3-4.9)*((41.3-4.9)*(T-41.3)-(41.3-47.5)*(41.3+4.9-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
