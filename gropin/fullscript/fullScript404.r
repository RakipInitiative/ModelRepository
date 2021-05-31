#############################
# start of Parameter script
#############################
T <- seq(-5.005,35.0649350649351,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 404 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(8.9*(10^-1)*(T-35.1)*((T+5)^2))/((31.6+5)*((31.6+5)*(T-31.6)-(31.6-35.1)*(31.6-5-2*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 404 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas psychrophiles in/on Tryptic Soy Broth
(gropin ID:404)')
#############################
# End of Visualisation script
#############################
