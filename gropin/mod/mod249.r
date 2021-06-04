############################# 
# start of Model script Gropin ID 249 
#############################
 
# constant coefficients for this model
a <- 46.6162
Ea <- 109.911
R <- 8.3140000000000002E-3
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.302585*exp(a-(Ea/R)*(1/(T+273)))/24

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
