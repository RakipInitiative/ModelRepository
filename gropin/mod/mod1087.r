############################# 
# start of Model script Gropin ID 1087 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,Sugar)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,Sugar) {
   mumax <-(-50.035+1.409*Sugar-0.010*(Sugar^2)+3.765*pH-0.162*(pH^2)-0.041*Sugar*pH)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['Sugar']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
