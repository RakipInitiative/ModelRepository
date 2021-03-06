############################# 
# start of Model script Gropin ID 1049 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,S)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,S) {
   mumax <-(-1.4*(1-1.9/pH)*(1-exp(1-pH/10.4))*(1-S/980))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['S']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
