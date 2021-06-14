############################# 
# start of Model script Gropin ID 158 
#############################
 
# constant coefficients for this model
d <- 0.0013
awmin <- 0.9459
Tmin <- -1.34
CO2max <- 7972
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(1/(d*(T-Tmin)*sqrt((aw-awmin)*(CO2max-CO2_dissolved_))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
