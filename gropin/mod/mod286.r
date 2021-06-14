############################# 
# start of Model script Gropin ID 286 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Phe,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,Phe,NaCl) {
   mumax <-(-0.0639+0.0137*T+0.0103*Phe+0.0257*NaCl-0.0001*T*Phe-0.0018*T*NaCl-0.0011*Phe*NaCl+(0.0004*(T^2))-(0.0002*(Phe^2))-(0.0011*(NaCl^2)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Phe'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
