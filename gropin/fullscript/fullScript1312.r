#############################
# start of Parameter script
#############################
T <- seq(4.004,27.972027972028,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1312 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-9.90*(10^-3)*(T+17.5)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1312 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Pseudomonas spp. in/on Button mushroom _Agaricus bisporus_
(gropin ID:1312)')
#############################
# End of Visualisation script
#############################
