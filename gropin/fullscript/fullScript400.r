#############################
# start of Parameter script
#############################
T <- seq(4.4044,45.954045954046,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 400 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.7*(T-46)*((T-4.4)^2))/((40.3-4.4)*((40.3-4.4)*(T-40.3)-(40.3-46)*(40.3+4.4-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 400 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas aeruginosa in/on Tryptic Soy Broth
(gropin ID:400)')
#############################
# End of Visualisation script
#############################
