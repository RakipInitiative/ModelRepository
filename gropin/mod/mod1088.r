############################# 
# start of Model script Gropin ID 1088 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,Sugar)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,Sugar) {
   mumax <-(-2221.43+85.390*Sugar-0.700*(Sugar^2)-419.81*pH+45.270*(pH^2)+2.45*Sugar*pH)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['Sugar']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
