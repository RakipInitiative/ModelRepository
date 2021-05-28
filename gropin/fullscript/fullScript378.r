#############################
# start of Parameter script
#############################
T <- seq(30.7692307692308,65.5655,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 378 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.8*(10^-2)*(T-65.5)*((T-30.8)^2))/((57.2-30.8)*((57.2-30.8)*(T-57.2)-(57.2-65.5)*(57.2+30.8-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 378 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
