############################# 
# start of Model script Gropin ID 156 
#############################
 
# constant coefficients for this model
b <- 0.000578
awmin <- 0.9544
Tmin <- -9.0299999999999994
CO2max <- 6691
NaLmax <- 5.87
 
variables <- data.frame(T,aw,CO2_dissolved_,NaL)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_,NaL) {
   mumax <-(b*(T-Tmin)*sqrt((aw-awmin)*(CO2max-CO2_dissolved_)*(NaLmax-NaL)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_'],argumentsPar['NaL']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
