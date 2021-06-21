############################# 
# start of Model script Gropin ID 153 
#############################
 
# constant coefficients for this model
d <- 9.3e-07
awmin <- 0.947
Tmin <- -2.38
CO2max <- 14000
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(1/(d*(aw-awmin)*(CO2max-CO2_dissolved_)*((T-Tmin)^2)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
