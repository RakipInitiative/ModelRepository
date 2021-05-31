#############################
# start of Parameter script
#############################
T <- seq(10.01,33.966033966034,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 256 
#############################
 
# constant coefficients for this model
a <- 0.0442
Tmin <- 5.407
 
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
# start of Visualisation script Gropin ID 256 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Staphylococcus aureus in/on Fresh milk
(gropin ID:256)')
#############################
# End of Visualisation script
#############################
