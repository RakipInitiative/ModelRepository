############################# 
# start of Model script Gropin ID 337 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-(-0.579+0.011*NaCl+0.163*pH+0.010*T-0.007*NaCl*pH-0.0006*NaCl*T+0.001*pH*T+0.003*(NaCl^2)-0.014*(pH^2)-0.0002*(T^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
