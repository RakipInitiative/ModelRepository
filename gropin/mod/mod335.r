############################# 
# start of Model script Gropin ID 335 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-(-541.43+(3.2203*(10^5)/(273+T))-(4.9081*(10^7)/((273+T)^2))+0.1033*NaCl-0.0523*(NaCl^2)+3.9848*pH-0.3115*(pH^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
