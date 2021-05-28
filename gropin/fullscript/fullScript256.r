#############################
# start of Parameter script
#############################
T <- seq(9.99000999000999,34.034,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 256 
#############################
 
# constant coefficients for this model
a <- 0.0442
Tmin <- 5.407
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(a*(T-Tmin))^2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 256 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
