#############################
# start of Parameter script
#############################
T <- seq(13.3866133866134,51.051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 380 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.2*(T-51)*((T-13.4)^2))/((38.7-13.4)*((38.7-13.4)*(T-38.7)-(38.7-51)*(38.7+13.4-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 380 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
