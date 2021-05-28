#############################
# start of Parameter script
#############################
T <- seq(3.996003996004,43.043,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 500 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.91*10^-2)*(T-(10^-1))*(1-exp((2.24*10^-2)*(T-48.2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 500 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
