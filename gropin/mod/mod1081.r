############################# 
# start of Model script Gropin ID 1081 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Time,TA,SS)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Time,TA,SS) {
   mumax <-(-14.54+(0.64*T)+0.40*Time-0.21*TA-0.02*SS)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Time'],argumentsPar['TA'],argumentsPar['SS']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
