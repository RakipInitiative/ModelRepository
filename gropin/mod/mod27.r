############################# 
# start of Model script Gropin ID 27 
#############################
 
# constant coefficients for this model
c <- 0.143
awmin <- 0.974
Tmin <- -0.48499999999999999
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(1/(c*sqrt(aw-awmin)*(T-Tmin)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
