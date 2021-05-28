#############################
# start of Parameter script
#############################
T <- seq(0,25.025,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 251 
#############################
 
# constant coefficients for this model
a <- 0.023
Tmin <- -4.6
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.302585*(a*(T-Tmin))^2/24

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 251 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
