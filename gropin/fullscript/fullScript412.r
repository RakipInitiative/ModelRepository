#############################
# start of Parameter script
#############################
T <- seq(2.6973026973027,19.5195,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 412 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.1*(10^-1)*(T-19.5)*((T-2.7)^2))/((15.6-2.7)*((15.6-2.7)*(T-15.6)-(15.6-19.5)*(15.6+2.7-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 412 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
