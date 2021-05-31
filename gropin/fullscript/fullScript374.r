#############################
# start of Parameter script
#############################
T <- seq(5.1051,43.956043956044,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 374 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(8.7*(10^-3)*(T-44)*((T-5.1)^2))/((35.4-5.1)*((35.4-5.1)*(T-35.4)-(35.4-44)*(35.4+5.1-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 374 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Aeromonas spp. in/on Nutrient broth
(gropin ID:374)')
#############################
# End of Visualisation script
#############################
