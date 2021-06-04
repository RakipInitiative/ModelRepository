############################# 
# start of Model script Gropin ID 1161 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-((sqrt(0.048^2)*sqrt((T-9.74)^2)*sqrt((sqrt(1-exp(0.208*(T-52.1))))^2))^(-1))*log(1+(1/0.0105))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
