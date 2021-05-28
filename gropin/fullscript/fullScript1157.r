#############################
# start of Parameter script
#############################
T <- seq(9.99000999000999,51.051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1157 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(0.050^2)*((T-10.15)^2)*(sqrt(1-exp(0.238*(T-53))))^2

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1157 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
