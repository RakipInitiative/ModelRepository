#############################
# start of Parameter script
#############################
T <- seq(10.989010989011,45.8458,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 385 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.1*(T-45.8)*((T-11)^2))/((39.3-11)*((39.3-11)*(T-39.3)-(39.3-45.8)*(39.3+11-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 385 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
