#############################
# start of Parameter script
#############################
T <- seq(2.2977022977023,39.7397,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 392 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.4*(10^-1)*(T-39.7)*((T-2.3)^2))/((30.8-2.3)*((30.8-2.3)*(T-30.8)-(30.8-39.7)*(30.8+2.3-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 392 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
