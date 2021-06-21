############################# 
# start of Model script Gropin ID 1181 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl,NaNO2) {
   mumax <-148.6-7.818*T-17.035*pH-0.084*NaNO2+0.185*T*T+1.261*pH*pH+0.00041*NaNO2*NaNO2

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
