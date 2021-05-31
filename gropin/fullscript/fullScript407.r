#############################
# start of Parameter script
#############################
T <- seq(-0.5005,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 407 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.3*(10^-3)*(T-34.4)*((T+0.5)^2))/((30+0.5)*((30+0.5)*(T-30)-(30-34.4)*(30-0.5-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 407 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas spp. in/on Nutrient broth
(gropin ID:407)')
#############################
# End of Visualisation script
#############################
