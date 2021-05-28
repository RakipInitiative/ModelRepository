############################# 
# start of Model script Gropin ID 1318 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,NaCl) {
   mumax <-7.45*((T-12.20)^2)*(1-exp(0.095*(T-54.47)))*( (1-NaCl*(5.2471+0.12206*NaCl)/1000)-0.9755)*(2-(1-NaCl*(5.2471+0.12206*NaCl)/1000)-0.9755)*(6.25-4.76)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaCl']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
