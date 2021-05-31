#############################
# start of Parameter script
#############################
T <- seq(7.9079,42.957042957043,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1168 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(0.911*(T-53.69)*((T-7.90)^2))/(((37.56-7.90)*(T-37.56)-(37.56-53.69)*(37.56+7.90-2*T))*(37.56-7.90))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1168 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Total Viable Counts in/on Oyster _raw_
(gropin ID:1168)')
#############################
# End of Visualisation script
#############################
