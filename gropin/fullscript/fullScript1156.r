#############################
# start of Parameter script
#############################
T <- seq(13.86385,46.953046953047,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1156 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.047*(T-12.7)*(1-exp(0.174*(T-48)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1156 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Clostridium sporogenes in/on Ground beef _cooked_
(gropin ID:1156)')
#############################
# End of Visualisation script
#############################
