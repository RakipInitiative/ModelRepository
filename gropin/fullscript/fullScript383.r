#############################
# start of Parameter script
#############################
T <- seq(-9.7097,26.7732267732268,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 383 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.5*(10^-1)*(T-26.8)*((T+9.7)^2))/((21.8+9.7)*((21.8+9.7)*(T-21.8)-(21.8-26.8)*(21.8-9.7-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 383 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Candida spp. in/on Ox muscle
(gropin ID:383)')
#############################
# End of Visualisation script
#############################
