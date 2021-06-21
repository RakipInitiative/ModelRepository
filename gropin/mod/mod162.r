############################# 
# start of Model script Gropin ID 162 
#############################
 
# constant coefficients for this model
a <- 0.00071277
awmin <- 0.9295
Tmin <- -3.5419
CO2max <- 3140
NaLmax <- 5.9547
 
variables <- data.frame(T,CO2_dissolved_,aw,NaL)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CO2_dissolved_,aw,NaL) {
   mumax <-a*(T-Tmin)*sqrt((aw-awmin)*(CO2max-CO2_dissolved_)*(NaLmax-NaL))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2_dissolved_'],argumentsPar['aw'],argumentsPar['NaL']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
