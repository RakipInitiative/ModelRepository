############################# 
# start of Model script Gropin ID 382 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.2*(10^-2)*(T-33.1)*((T+7.5)^2))/((27.8+7.5)*((27.8+7.5)*(T-27.8)-(27.8-33.1)*(27.8-7.5-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
