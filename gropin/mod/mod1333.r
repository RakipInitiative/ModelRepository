############################# 
# start of Model script Gropin ID 1333 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH) {
   mumax <-0.020*(pH+0.57)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
