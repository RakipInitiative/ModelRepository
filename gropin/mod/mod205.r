############################# 
# start of Model script Gropin ID 205 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl,pH,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,NaCl,pH,NaNO2) {
   mumax <-20.1394-0.2256*T+0.0186*NaCl-4.3533*pH+0.0272*NaNO2-0.00359*pH*NaNO2+0.00322*(T^2)+0.3043*(pH^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaCl'],argumentsPar['pH'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
