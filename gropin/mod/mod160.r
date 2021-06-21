############################# 
# start of Model script Gropin ID 160 
#############################
 
# constant coefficients for this model
ll <- 304.49
l1 <- -24.22
l2 <- -299.62
l3 <- 27.63
l4 <- 0.00136
l5 <- 0.0377
l7 <- 0.2
l9 <- 23.68
l10 <- -0.18
l11 <- -0.00017
l12 <- -27.14
l14 <- 0.00073
 
variables <- data.frame(T,NaL,aw,CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,NaL,aw,CO2) {
   mumax <-(ll+(l1*T)+(l2*aw)+(l3*NaL)+(l4*CO2)+(l5*(T^2))+(l7*(NaL^2))+(l9*T*aw)+(l10*T*NaL)+(l11*T*CO2)+(l12*aw*NaL)+(l14*NaL*CO2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaL'],argumentsPar['aw'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
