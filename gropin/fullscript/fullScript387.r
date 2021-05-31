#############################
# start of Parameter script
#############################
T <- seq(5.6056,40.2597402597403,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 387 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.4*(T-47.3)*((T-5.6)^2))/((40.3-5.6)*((40.3-5.6)*(T-40.3)-(40.3-47.3)*(40.3+5.6-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 387 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Escherichia coli in/on Nutrient broth
(gropin ID:387)')
#############################
# End of Visualisation script
#############################
