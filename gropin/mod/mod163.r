############################# 
# start of Model script Gropin ID 163 
#############################
 
# constant coefficients for this model
b <- 4.5231e-07
awmin <- 0.9485
Tmin <- -1.5389999999999999
CO2max <- 2476
NaLmax <- 3.7494
 
variables <- data.frame(T,CO2_dissolved_,aw,NaL)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CO2_dissolved_,aw,NaL) {
   mumax <-(1/(b*(aw-awmin)*(CO2max-CO2_dissolved_)*(T-Tmin)*(NaLmax-NaL)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2_dissolved_'],argumentsPar['aw'],argumentsPar['NaL']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
