############################# 
# start of Model script Gropin ID 235 
#############################
 
# constant coefficients for this model
pHmin <- 3.88
pHopt <- 7.2
pHmax <- 12.17
Tmin <- 3.06
Topt <- 41.1
Tmax <- 45.06
mopt <- 2.635
 
variables <- data.frame(pH,T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,T) {
   mumax <-2.635*(((pH-pHmin)*(pH-pHmax)/(((pH-pHmin)*(pH-pHmax))-(pH-pHopt)^2)))*((T-Tmax)*(T-Tmin)^2)/((Topt-Tmin)*((Topt-Tmin)*(T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
