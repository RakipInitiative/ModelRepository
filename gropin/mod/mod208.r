############################# 
# start of Model script Gropin ID 208 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl,NaNO2) {
   mumax <-log(2)/exp(21.2574-0.2643*T+0.00404*NaCl-5.2054*pH+0.0189*NaNO2+0.00709*T*pH-0.00252*pH*NaNO2+0.00265*(T^2)+0.000129*(NaCl^2)+0.3746*(pH^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
