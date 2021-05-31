#############################
# start of Parameter script
#############################
T <- seq(0.1001,37.2627372627373,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 373 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(8.3*(10^-3)*(T-37.3)*((T-0.1)^2))/((30.2-0.1)*((30.2-0.1)*(T-30.2)-(30.2-37.3)*(30.2+0.1-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 373 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Acinetobacter spp. in/on Nutrient broth
(gropin ID:373)')
#############################
# End of Visualisation script
#############################
