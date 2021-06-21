############################# 
# start of Model script Gropin ID 196 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl,NaNO2) {
   mumax <-log(2)/exp(13.5104-(0.1033*T)+(0.0423*NaCl)-(3.3464*pH)+(0.022*NaNO2)-(0.0000353*T*NaCl)-(0.0142*T*pH)+(0.00000483*T*NaNO2)-(0.0036*NaCl*pH)+(0.00000414*NaCl*NaNO2)-(0.00282*pH*NaNO2)+(0.00273*(T^2))-(0.000271*(NaCl^2))+(0.2629*(pH^2))-(0.00000086*(NaNO2^2)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
