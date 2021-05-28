#############################
# start of Parameter script
#############################
T <- seq(11.1888111888112,48.048,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 388 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.1*(T-48)*((T-11.2)^2))/((41-11.2)*((41-11.2)*(T-41)-(41-48)*(41+11.2-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 388 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
