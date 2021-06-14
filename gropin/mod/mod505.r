############################# 
# start of Model script Gropin ID 505 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-(-13.616+0.203*T+3.0026*pH+8.42249*sqrt(1-aw)-0.00629*T*pH+0.11391*T*sqrt(1-aw)-0.00217*(T^2)-0.2171*(pH^2)-93.4381*((sqrt(1-aw))^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
