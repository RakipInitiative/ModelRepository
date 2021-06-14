############################# 
# start of Model script Gropin ID 216 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Phe,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,Phe,NaCl) {
   mumax <-(61.354-11.657*T+10.159*Phe+14.752*NaCl-0.147*(T*Phe)-0.863*(T*NaCl)+0.423*(Phe*NaCl)+0.321*(T^2)-0.231*(Phe^2)+0.485*(NaCl^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Phe'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
