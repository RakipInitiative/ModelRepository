#############################
# start of Parameter script
#############################
T <- seq(5.4945054945055,50.05,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 366 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(0.0435*(T-2.9)*(1-exp(0.314*(T-49.2))))^2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 366 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
