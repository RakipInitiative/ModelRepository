############################# 
# start of Model script Gropin ID 828 
#############################
 
# constant coefficients for this model
pHmin <- 2.93
pHopt <- 4.22
pHmax <- 5.9
Tmin <- 18.11
Topt <- 48.6
Tmax <- 55.68
mopt <- 1.035
 
variables <- data.frame(pH,T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,T) {
   mumax <-1.035*((pH-pHmax)*(pH-pHmin)/((pHopt-pHmin)*(pH-pHopt)-(pHopt-pHmax)*(pHmin-pH)))*((T-Tmax)*((T-Tmin)^2)/((Topt-Tmin)*((Topt-Tmin)*(T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
