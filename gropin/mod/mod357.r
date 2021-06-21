############################# 
# start of Model script Gropin ID 357 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw,Gelatin)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,aw,Gelatin) {
   mumax <-4.455*sqrt(aw-0.9488)*sqrt(1-10^(4.135-pH))*sqrt(0.6313+(1-0.6313)*(0.4027/(0.4027+Gelatin)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Gelatin']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
