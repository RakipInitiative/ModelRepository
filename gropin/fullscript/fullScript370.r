#############################
# start of Parameter script
#############################
T <- seq(-7.8078,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 370 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.8*(10^-1)*(T-30)*((T+7.8)^2))/((24.6+7.8)*((24.6+7.8)*(T-24.6)-(24.6-30)*(24.6-7.8-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 370 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Achromobacter spp. in/on Ox muscle
(gropin ID:370)')
#############################
# End of Visualisation script
#############################
