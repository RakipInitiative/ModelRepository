############################# 
# start of Model script Gropin ID 1371 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp(2.682-5.124*sqrt(1-aw)+26.62*(sqrt(1-aw))^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
