############################# 
# start of Model script Gropin ID 1338 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(-0.2515+3.887*bw-21.35*bw^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
