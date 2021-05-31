#############################
# start of Parameter script
#############################
T <- seq(-2.8028,35.4645354645355,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 390 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.3*(10^-3)*(T-35.5)*((T+2.8)^2))/((29.3+2.8)*((29.3+2.8)*(T-29.3)-(29.3-35.5)*(29.3-2.8-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 390 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Flavobacterium spp. in/on Nutrient broth
(gropin ID:390)')
#############################
# End of Visualisation script
#############################
