############################# 
# start of Model script Gropin ID 437 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-(-0.2554-9499.95*(1/(T+273))-7.22*NaCl+8.85*pH-0.6114*(pH^2)+1615.8*(1/(T+273))*NaCl+0.1336*NaCl*pH)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
