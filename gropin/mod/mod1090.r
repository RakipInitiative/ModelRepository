############################# 
# start of Model script Gropin ID 1090 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,Sugar)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,Sugar) {
   mumax <-2771.561-75.864*Sugar+0.573*(Sugar^2)-228.12*pH+27.544*(pH^2)+0.956*Sugar*pH

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['Sugar']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
