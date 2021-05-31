#############################
# start of Parameter script
#############################
T <- seq(4.004,42.957042957043,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1166 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.032*(T-1.471)*(1-exp(0.129*(T-51.82)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1166 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Total Viable Counts in/on Oyster _raw_
(gropin ID:1166)')
#############################
# End of Visualisation script
#############################
