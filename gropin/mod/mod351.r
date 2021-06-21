############################# 
# start of Model script Gropin ID 351 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Oleo)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,Oleo) {
   mumax <-(-5.348+0.162*T+0.319*pH+0.066*Oleo-0.0061*T*pH-0.00091*(T^2)-0.561*(Oleo^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Oleo']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
