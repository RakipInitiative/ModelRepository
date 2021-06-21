############################# 
# start of Model script Gropin ID 151 
#############################
 
# constant coefficients for this model
Im <- 0.9
m1 <- -0.61
m2 <- 4.8999999999999998E-4
m3 <- -0.91
m4 <- 0.05
m5 <- 0.05
m6 <- 0.05
m7 <- -2.3e-06
m8 <- 0.63
m9 <- -5e-04
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(Im+(m1*T)+(m2*CO2_dissolved_)+(m3*aw)+(m4*(T^2))+(m5*(CO2_dissolved_^2))+(m6*(aw^2))+(m7*T*CO2_dissolved_)+(m8*T*aw)+(m9*CO2_dissolved_*aw))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
