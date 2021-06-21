############################# 
# start of Model script Gropin ID 24 
#############################
 
# constant coefficients for this model
b <- 1.17e-05
awmin <- 0.9715
Tmin <- -5.68
CO2max <- 2300
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
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
