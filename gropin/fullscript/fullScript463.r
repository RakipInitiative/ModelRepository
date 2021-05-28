#############################
# start of Parameter script
#############################
T <- seq(11.988011988012,40.04,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 463 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-1.43*(10^-1)*(T-1.63)*(1-exp(1.45*(10^-1)*(T-46.67)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 463 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
