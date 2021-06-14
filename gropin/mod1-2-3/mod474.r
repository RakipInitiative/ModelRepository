############################# 
# start of Model script Gropin ID 474 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Irradiationdose)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,Irradiationdose) {
   mumax <-5.38-0.94*T+20.2*Irradiationdose+0.04*(T^2)-0.66*(Irradiationdose^2)-2.64*T*Irradiationdose+0.045*T*(Irradiationdose^2)+0.091*Irradiationdose*(T^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Irradiationdose']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
