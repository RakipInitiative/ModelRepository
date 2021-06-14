############################# 
# start of Model script Gropin ID 263 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Ac)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,Ac) {
   mumax <-(0.046*(T+5.260)*sqrt(1-10^(3.793-pH))*sqrt(1-sqrt((Ac/(1+10^(pH-4.76)))/7.59)))^2/24

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Ac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
