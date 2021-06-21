############################# 
# start of Model script Gropin ID 209 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl,pH,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,NaCl,pH,NaNO2) {
   mumax <-26.0899-0.1901*T+0.0545*NaCl-6.3831*pH+0.0167*NaNO2+0.000201*T*NaCl+0.0000232*T*NaNO2-0.00729*NaCl*pH-0.00229*pH*NaNO2+0.0019*(T^2)+0.000098*(NaCl^2)+0.4784*(pH^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaCl'],argumentsPar['pH'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
