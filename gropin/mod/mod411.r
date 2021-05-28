############################# 
# start of Model script Gropin ID 411 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.5*(10^-1)*(T-81.8)*((T-36.8)^2))/((70.9-36.8)*((70.9-36.8)*(T-70.9)-(70.9-81.8)*(70.9+36.8-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
