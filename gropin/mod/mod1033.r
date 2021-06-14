############################# 
# start of Model script Gropin ID 1033 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Fructose,Ethanol,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(Fructose,Ethanol,pH) {
   mumax <-log(0.45)-0.0146*((pH-7)^2)-40.85*((sqrt(1-( 55.556/(55.556+(Ethanol*0.7893/46)+(Fructose*10/180.16))))-0.05)^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Fructose'],argumentsPar['Ethanol'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
