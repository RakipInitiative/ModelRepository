############################# 
# start of Model script Gropin ID 1189 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(-0.417+(30.548)* sqrt(1-aw)-92.980*( sqrt(1-aw)* sqrt(1-aw))+0.242*T+0.0018*T*T-0.849* sqrt(1-aw)*T)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
