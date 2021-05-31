#############################
# start of Parameter script
#############################
T <- seq(5.005,46.1538461538462,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 399 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.1*(T-46.2)*((T-5)^2))/((38.1-5)*((38.1-5)*(T-38.1)-(38.1-46.2)*(38.1+5-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 399 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas aeruginosa in/on Nutrient broth
(gropin ID:399)')
#############################
# End of Visualisation script
#############################
