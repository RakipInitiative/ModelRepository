############################# 
# start of Model script Gropin ID 1172 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.21*(T-46.8)*((T-8.23)^2)/(((37.6-8.23)*(T-37.6)-(37.6-46.8)*(37.6+8.23-2*T))*(37.6-8.23))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
