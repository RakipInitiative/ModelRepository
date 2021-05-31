#############################
# start of Parameter script
#############################
T <- seq(0,49.95004995005,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1091 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.068*(T-68.14)*((T-33.76)^2)/((28.06)*((28.06)*(T-61.82)-(-604.0656+12.64*T)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1091 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Geobacillus stearothermophilus in/on Milk _evaporated_
(gropin ID:1091)')
#############################
# End of Visualisation script
#############################
