############################# 
# start of Model script Gropin ID 492 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,days)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,days) {
   mumax <-0.89+0.081*days+0.11*T+0.000192*(days^2)-0.0034*(T^2)+0.0068*T*days

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['days']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
