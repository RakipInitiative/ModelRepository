#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 495 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.1457-0.011*T+0.001*(T^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 495 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Ham _cooked_
(gropin ID:495)')
#############################
# End of Visualisation script
#############################
