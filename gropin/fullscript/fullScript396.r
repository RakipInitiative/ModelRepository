#############################
# start of Parameter script
#############################
T <- seq(0.1001,35.1648351648352,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 396 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3.4*(10^-1)*(T-35.2)*((T-0.1)^2))/((28.3-0.1)*((28.3-0.1)*(T-28.3)-(28.3-35.2)*(28.3+0.1-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 396 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Micrococcus cryophilus in/on Various
(gropin ID:396)')
#############################
# End of Visualisation script
#############################
