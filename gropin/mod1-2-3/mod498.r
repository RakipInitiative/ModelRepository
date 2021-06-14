############################# 
# start of Model script Gropin ID 498 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-0.46*(((T-27.19)*((T+4.8)^2))/(28.19*(28.19*(T-23.39)-(-3.8)*(18.59-2*T))))*(1/6.6)*((pH-4.79)*(9.93-pH))*(1/335.26)*((NaCl+13.62)*(23-NaCl))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
