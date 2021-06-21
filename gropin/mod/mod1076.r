############################# 
# start of Model script Gropin ID 1076 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(O2) {
   mumax <-0.674*(1-(O2/3.26)^2.983)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['O2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
