#############################
# start of Parameter script
#############################
T <- seq(-4.995004995005,34.3343,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 406 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(9.6*(10^-1)*(T-34.3)*((T+5)^2))/((29.8+5)*((29.8+5)*(T-29.8)-(29.8-34.3)*(29.8-5-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 406 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
