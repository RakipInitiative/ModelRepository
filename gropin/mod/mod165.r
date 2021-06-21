############################# 
# start of Model script Gropin ID 165 
#############################
 
# constant coefficients for this model
Im <- -1.0636
m1 <- -0.1469
m2 <- 5.8168E-4
m3 <- 1.1532
m4 <- -0.00028161
m9 <- 0.1733
m10 <- -0.0025119
m11 <- -4.4792e-06
m13 <- -0.00060063
 
variables <- data.frame(T,CO2_dissolved_,aw,NaL)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CO2_dissolved_,aw,NaL) {
   mumax <-Im+(m1*T)+(m2*CO2_dissolved_)+(m3*aw)+(m4*NaL)+(m9*T*aw)+(m10*T*NaL)+(m11*T*CO2_dissolved_)+(m13*aw*CO2_dissolved_)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2_dissolved_'],argumentsPar['aw'],argumentsPar['NaL']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
