############################# 
# start of Model script Gropin ID 380 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.2*(T-51)*((T-13.4)^2))/((38.7-13.4)*((38.7-13.4)*(T-38.7)-(38.7-51)*(38.7+13.4-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
