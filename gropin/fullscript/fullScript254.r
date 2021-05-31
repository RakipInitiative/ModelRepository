#############################
# start of Parameter script
#############################
T <- seq(3.003,14.985014985015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 254 
#############################
 
# constant coefficients for this model
a <- 0.017
Tmin <- -6.23
 
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
# start of Visualisation script Gropin ID 254 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Traditional custard
(gropin ID:254)')
#############################
# End of Visualisation script
#############################
