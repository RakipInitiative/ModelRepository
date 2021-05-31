#############################
# start of Parameter script
#############################
T <- seq(3.8038,18.4815184815185,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 414 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.1*(10^-1)*(T-18.5)*((T-3.8)^2))/((12.4-3.8)*((12.4-3.8)*(T-12.4)-(12.4-18.5)*(12.4+3.8-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 414 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Vibrio psychroerythrus in/on Nutrient broth
(gropin ID:414)')
#############################
# End of Visualisation script
#############################
