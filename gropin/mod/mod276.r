############################# 
# start of Model script Gropin ID 276 
#############################
 
# constant coefficients for this model
mref <- 0.065
dco2 <- 0.0103
Ea <- 70.3
 
variables <- data.frame(T,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CO2) {
   mumax <-(log(mref)-(dco2*CO2)+(Ea/0.00831)*((1/273)-(1/(T+273))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
