############################# 
# start of Model script Gropin ID 467 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(-0.07323+0.00168*T+0.03188*pH-0.000038*T*pH+0.000017*(T^2)-0.00333*(pH^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
