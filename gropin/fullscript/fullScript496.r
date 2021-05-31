#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 496 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.13770-0.0118*T+0.0011*(T^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 496 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Ham _cooked_
(gropin ID:496)')
#############################
# End of Visualisation script
#############################
