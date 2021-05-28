#############################
# start of Parameter script
#############################
T <- seq(12.0879120879121,62.9629,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 376 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1*(T-62.9)*((T-12.1)^2))/((53.8-12.1)*((53.8-12.1)*(T-53.8)-(53.8-62.9)*(53.8+12.1-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 376 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
