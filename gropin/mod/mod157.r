############################# 
# start of Model script Gropin ID 157 
#############################
 
# constant coefficients for this model
Im <- -54.62
m1 <- -0.78
m2 <- 111.73
m3 <- 0.73
m4 <- 0.000652
m5 <- -0.0013
m6 <- -57.05
m9 <- 0.84
m10 <- -0.000856
m11 <- -1.65e-06
m12 <- -0.77
m13 <- -0.000677
 
variables <- data.frame(T,aw,NaL,CO2_dissolved_)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,NaL,CO2_dissolved_) {
   mumax <-(Im+(m1*T)+(m2*aw)+(m3*NaL)+(m4*CO2_dissolved_)+(m5*(T^2))+(m6*(aw^2))+(m9*T*aw)+(m10*T*NaL)+(m11*T*CO2_dissolved_)+(m12*aw*NaL)+(m13*aw*NaL))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['NaL'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
