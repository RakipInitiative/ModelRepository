############################# 
# start of Model script Gropin ID 1293 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CO2) {
   mumax <-0.082+0.032*T-3.6*(10^-4)-1.1*(10^-4)*T*CO2

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
