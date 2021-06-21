############################# 
# start of Model script Gropin ID 1188 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(-0.125+(22.690)* sqrt(1-aw)-71.360*( sqrt(1-aw)* sqrt(1-aw))+0.267*T+0.0015*T*T-0.877* sqrt(1-aw)*T)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
