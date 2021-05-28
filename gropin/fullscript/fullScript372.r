#############################
# start of Parameter script
#############################
T <- seq(1.3986013986014,35.1351,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 372 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.3*(10^-3)*(T-35.1)*((T-1.4)^2))/((28.4-1.4)*((28.4-1.4)*(T-28.4)-(28.4-35.1)*(28.4+1.4-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 372 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
