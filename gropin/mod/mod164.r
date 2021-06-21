############################# 
# start of Model script Gropin ID 164 
#############################
 
# constant coefficients for this model
I <- 51.1874
I1 <- -0.4967
I2 <- 6.0667999999999996E-4
I3 <- -44.8051
I4 <- 0.5566
I5 <- 0.014918
I8 <- -0.048613
I14 <- 0.00031252
 
variables <- data.frame(T,CO2_dissolved_,aw,NaL)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CO2_dissolved_,aw,NaL) {
   mumax <-I+(I1*T)+(I2*CO2_dissolved_)+(I3*aw)+(I4*NaL)+(I5*(T^2))+(I8*(NaL^2))+(I14*NaL*CO2_dissolved_)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2_dissolved_'],argumentsPar['aw'],argumentsPar['NaL']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
