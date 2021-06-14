############################# 
# start of Model script Gropin ID 26 
#############################
 
# constant coefficients for this model
Im <- 74.6
m1 <- -0.74
m2 <- 5.7000000000000002E-3
m3 <- -151
m4 <- 0.000124
m5 <- 4.62e-08
m6 <- 77
m7 <- -1e-05
m8 <- 0.76
m9 <- -0.0057
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
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
