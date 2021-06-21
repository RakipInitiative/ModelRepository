############################# 
# start of Model script Gropin ID 30 
#############################
 
# constant coefficients for this model
Il <- -18000
I1 <- -35.6
I2 <- 0.47
I3 <- 37000
I4 <- 0.053
I5 <- 7.6e-08
I6 <- -19000
I7 <- -0.00035
I8 <- 34
I9 <- -0.46
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(Il+(I1*T)+(I2*CO2_dissolved_)+(I3*aw)+(I4*(T^2))+(I5*(CO2_dissolved_^2))+(I6*(aw^2))+(I7*T*CO2_dissolved_)+(I8*T*aw)+(I9*CO2_dissolved_*aw))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
