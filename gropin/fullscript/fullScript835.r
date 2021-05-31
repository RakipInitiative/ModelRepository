#############################
# start of Parameter script
#############################
T <- seq(35.035,66.9330669330669,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 835 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.068*(((T-68.14)*((T-33.76)^2))/((61.82-33.76)*((61.82-33.76)*(T-61.82)-(61.82-68.14)*(61.82+33.76-2*T))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 835 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Geobacillus stearothermophilus in/on Milk _evaporated_
(gropin ID:835)')
#############################
# End of Visualisation script
#############################
