############################# 
# start of Model script Gropin ID 1345 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp(-0.1605+2.389*sqrt(1-aw)-17.08*(sqrt(1-aw))^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
