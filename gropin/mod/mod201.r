############################# 
# start of Model script Gropin ID 201 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl,pH,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,NaCl,pH,NaNO2) {
   mumax <-26.8685-0.2153*T+0.0516*NaCl-6.5596*pH+0.02*NaNO2+0.000223*T*NaCl+0.00368*T*pH+0.0000193*T*NaNO2-0.00686*NaCl*pH-0.0000037*NaCl*NaNO2-0.0028*pH*NaNO2+0.00192*(T^2)+0.000102*(NaCl^2)+0.4873*(pH^2)+0.00000074*(NaNO2^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaCl'],argumentsPar['pH'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
