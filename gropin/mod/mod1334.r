############################# 
# start of Model script Gropin ID 1334 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH) {
   mumax <-0.023*(pH-0.60)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
