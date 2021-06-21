############################# 
# start of Model script Gropin ID 1101 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-sqrt((1/10.3)* ((T-64.2)*((T-38.2)^2))/((53.6-38.2)* (((53.6-38.2)*(T-53.6)-(53.6-64.2)*(53.6+38.2-2*T)))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
