############################# 
# start of Model script Gropin ID 374 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(8.7*(10^-3)*(T-44)*((T-5.1)^2))/((35.4-5.1)*((35.4-5.1)*(T-35.4)-(35.4-44)*(35.4+5.1-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
