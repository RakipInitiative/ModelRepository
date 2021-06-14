############################# 
# start of Model script Gropin ID 497 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-0.64*(((T-30.85)*((T+3.36)^2))/(30.37*(30.37*(T-27.01)-(-3.84)*(23.65-2*T))))*(1/5.38)*((pH-4.79)*(9.43-pH)/3873.82)*((NaCl+62.24)*(62.24-NaCl))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
