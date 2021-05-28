############################# 
# start of Model script Gropin ID 353 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.366+3.003*exp(-((log(2)/(log(0.296)^2))*log((((T-42.330)*((0.296^2)-1))/(15.653*0.296))+1)^2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
