############################# 
# start of Model script Gropin ID 480 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,NO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,NO2) {
   mumax <-(-0.12635+0.027170*pH+0.031746*NO2+(-0.008778)*pH*NO2+(-0.005576)*(pH^2)+0.024220*(NO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['NO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
