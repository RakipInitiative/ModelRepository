############################# 
# start of Model script Gropin ID 23 
#############################
 
# constant coefficients for this model
a <- 0.032
awmin <- 0.9718
Tmin <- -5.25
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(a*(aw-awmin)*((T-Tmin)^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
