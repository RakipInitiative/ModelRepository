############################# 
# start of Model script Gropin ID 1275 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(O2,CO2) {
   mumax <-3.23-0.016*O2+0.051*CO2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['O2'],argumentsPar['CO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
