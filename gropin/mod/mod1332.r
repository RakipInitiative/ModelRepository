############################# 
# start of Model script Gropin ID 1332 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-2.14*((pH-3.84)*(pH-14.1)/((pH-3.84)*(pH-14.1)-((pH-6.47)^2)))*(((aw-0.939)/(0.992-0.939))^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
