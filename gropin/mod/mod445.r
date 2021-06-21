############################# 
# start of Model script Gropin ID 445 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,CO2,O2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,CO2,O2) {
   mumax <-(-4.50+(-0.829)*T+(-0.0151)*(T^2)+(-0.00122)*T*CO2+0.184*T*pH+(-0.00114)*pH*O2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['CO2'],argumentsPar['O2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
