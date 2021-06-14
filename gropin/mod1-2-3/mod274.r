############################# 
# start of Model script Gropin ID 274 
#############################
 
# constant coefficients for this model
mref <- 0.026
dm <- 0.535
Ea <- 95.8
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(log(mref)-dm*(5.7-pH)-(Ea/0.00831)*((1/(T+273))-(1/273)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
