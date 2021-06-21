############################# 
# start of Model script Gropin ID 504 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,La)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,La) {
   mumax <-(-18.851+0.2409*T+4.2628*pH+6.36771*sqrt(1-aw)-0.00232*(T^2)-0.31377*(pH^2)-43.1241*((sqrt(1-aw))^2)+La*(-3.5*(10^-5)-3*(10^-7)*T+0.000009*pH-0.00014*sqrt(1-aw)-1.2*(10^-9)*La))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['La']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
