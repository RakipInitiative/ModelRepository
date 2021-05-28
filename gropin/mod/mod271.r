############################# 
# start of Model script Gropin ID 271 
#############################
 
# constant coefficients for this model
mref <- 0.056
dm <- 0.451
Ea <- 69.3
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(log(mref)-dm*(5.7-pH)-(Ea/0.00831)*((1/(T+273))-(1/273)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
