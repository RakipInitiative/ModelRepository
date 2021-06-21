############################# 
# start of Model script Gropin ID 159 
#############################
 
# constant coefficients for this model
e <- 0.000849
awmin <- 0.9464
Tmin <- -1.25
CO2max <- 5591
NaLmax <- 5.04
 
variables <- data.frame(T,aw,CO2_dissolved_,NaL)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_,NaL) {
   mumax <-(1/(e*(T-Tmin)*sqrt((aw-awmin)*(CO2max-CO2_dissolved_)*(NaLmax-NaL))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_'],argumentsPar['NaL']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
