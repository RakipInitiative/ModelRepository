############################# 
# start of Model script Gropin ID 155 
#############################
 
# constant coefficients for this model
a <- 0.001187
awmin <- 0.9542
Tmin <- -8.81
CO2max <- 7799
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(a*(T-Tmin)*sqrt((aw-awmin)*(CO2max-CO2_dissolved_)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
