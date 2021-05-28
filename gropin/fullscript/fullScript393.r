#############################
# start of Parameter script
#############################
T <- seq(1.4985014985015,39.8398,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 393 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.4*(10^-1)*(T-39.8)*((T-1.5)^2))/((30.9-1.5)*((30.9-1.5)*(T-30.9)-(30.9-39.8)*(30.9+1.5-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 393 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
