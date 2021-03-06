############################# 
# start of Model script Gropin ID 1063 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Sugar,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(Sugar,pH) {
   mumax <-4993.00351-147.44162*Sugar+1.25367*(Sugar^2)-161.13055*pH+71.38168*(pH^2)-4.43598*Sugar*pH

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Sugar'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
