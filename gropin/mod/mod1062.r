############################# 
# start of Model script Gropin ID 1062 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Sugar,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(Sugar,pH) {
   mumax <-12.63699-0.50885*Sugar+0.00459363*(Sugar^2)+4.26298*pH-0.038048*(pH^2)-0.05637*Sugar*pH

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Sugar'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
