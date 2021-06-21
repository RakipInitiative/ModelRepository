############################# 
# start of Model script Gropin ID 1128 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,PL_SDAmix)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,PL_SDAmix) {
   mumax <-7.7668-0.4074*T+0.5026*PL_SDAmix+0.0016*T*PL_SDAmix+0.0063*(T^2)-0.0601*(PL_SDAmix^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['PL_SDAmix']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
