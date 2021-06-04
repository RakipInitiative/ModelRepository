############################# 
# start of Model script Gropin ID 1168 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(0.911*(T-53.69)*((T-7.90)^2))/(((37.56-7.90)*(T-37.56)-(37.56-53.69)*(37.56+7.90-2*T))*(37.56-7.90))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
