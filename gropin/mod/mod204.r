############################# 
# start of Model script Gropin ID 204 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl,NaNO2) {
   mumax <-log(2)/exp(13.5303-0.1096*T+0.0266*NaCl-3.2997*pH+0.0232*NaNO2-0.0134*T*pH-0.00332*NaCl*pH-0.00296*pH*NaNO2+0.00271*(T^2)+0.257*(pH^2)-0.00000085*(NaNO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
