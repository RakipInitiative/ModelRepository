############################# 
# start of Model script Gropin ID 28 
#############################
 
# constant coefficients for this model
d <- 0.00113
awmin <- 0.9638
Tmin <- -4.3
CO2max <- 2100
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(1/(d*sqrt(aw-awmin)*sqrt(CO2max-CO2_dissolved_)*(T-Tmin)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
