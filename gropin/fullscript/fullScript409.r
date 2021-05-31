#############################
# start of Parameter script
#############################
T <- seq(-2.9029,34.3656343656344,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 409 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3.7*(10^-3)*(T-34.4)*((T+2.9)^2))/((27.2+2.9)*((27.2+2.9)*(T-27.2)-(27.2-34.4)*(27.2-2.9-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 409 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas spp. in/on Nutrient broth
(gropin ID:409)')
#############################
# End of Visualisation script
#############################
