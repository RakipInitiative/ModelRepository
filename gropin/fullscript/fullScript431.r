#############################
# start of Parameter script
#############################
T <- seq(-0.4004,39.1608391608392,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 431 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.03772*(T+6.1)*(1-exp(0.1719*(T-41.2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 431 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas spp. in/on Nutrient broth
(gropin ID:431)')
#############################
# End of Visualisation script
#############################
