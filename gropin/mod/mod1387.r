############################# 
# start of Model script Gropin ID 1387 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CLO)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CLO) {
   mumax <-((48.5183/(T-(-1)))^0.5753)*(((100+CLO)/(100-CLO))^0.5913)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CLO']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
