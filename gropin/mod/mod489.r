############################# 
# start of Model script Gropin ID 489 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Fruct,NaCl,pH,Ac)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(Fruct,NaCl,pH,Ac) {
   mumax <-4.76+3.79*((Fruct-19.5)/17.7)+1.47*((NaCl-3.4)/1.1)-1.62*((pH-3.8)/0.4)+3.21*((Ac-2.3)/0.7)+0.59*((Fruct-19.5)/17.7)*((NaCl-3.4)/1.1)-0.44*((Fruct-19.5)/17.7)*((pH-3.8)/0.4)+0.82*((Fruct-19.5)/17.7)*((Ac-2.3)/0.7)-0.24*((NaCl-3.4)/1.1)*((pH-3.8)/0.4)+0.46*((NaCl-3.4)/1.1)*((Ac-2.3)/0.7)-0.74*((pH-3.8)/0.4)*((Ac-2.3)/0.7)+0.39*(((Ac-2.3)/0.7)^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Fruct'],argumentsPar['NaCl'],argumentsPar['pH'],argumentsPar['Ac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
