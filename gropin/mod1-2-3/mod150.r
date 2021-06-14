############################# 
# start of Model script Gropin ID 150 
#############################
 
# constant coefficients for this model
b <- 2.5e-06
awmin <- 0.956
Tmin <- -9
CO2max <- 6100
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(b*(aw-awmin)*(CO2max-CO2_dissolved_)*((T-Tmin)^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
