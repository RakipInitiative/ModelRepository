############################# 
# start of Model script Gropin ID 503 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-(-11.909+0.3649*T+(1.7832*pH)+7.00019*sqrt(1-aw)-0.00442*(T*pH)-0.00458*(T^2)-0.12539*(pH^2)-62.114*(sqrt(1-aw))^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
