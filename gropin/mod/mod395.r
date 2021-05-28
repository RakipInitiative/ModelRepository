############################# 
# start of Model script Gropin ID 395 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3.8*(10^-1)*(T-37.9)*((T-0.9)^2))/((30.9-0.9)*((30.9-0.9)*(T-30.9)-(30.9-37.9)*(30.9+0.9-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
