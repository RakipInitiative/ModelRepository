############################# 
# start of Model script Gropin ID 1160 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-((sqrt(0.050^2)*sqrt((T-10.15)^2)*sqrt((sqrt(1-exp(0.238*(T-53))))^2))^(-1))*log(1+(1/0.0699))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
