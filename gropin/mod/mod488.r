############################# 
# start of Model script Gropin ID 488 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(S_S,Ac,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(S_S,Ac,pH) {
   mumax <-(4.53+3.29*((S_S-1.66)/0.6)+2.94*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)+0.36*(((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)^2)+0.82*((S_S-1.66)/0.6)*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['S_S'],argumentsPar['Ac'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
