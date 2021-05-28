#############################
# start of Parameter script
#############################
T <- seq(-0.899100899100899,37.2372,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 397 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.3*(10^-3)*(T-37.2)*((T+0.9)^2))/((30.5+0.9)*((30.5+0.9)*(T-30.5)-(30.5-37.2)*(30.5-0.9-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 397 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
