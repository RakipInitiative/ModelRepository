############################# 
# start of Model script Gropin ID 1306 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.0362*(T-4.10)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
