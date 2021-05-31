#############################
# start of Parameter script
#############################
T <- seq(4.004,42.957042957043,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1167 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.072*((T-3.47)^0.75)*(1-exp(0.227*(T-50.11)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1167 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Total Viable Counts in/on Oyster _raw_
(gropin ID:1167)')
#############################
# End of Visualisation script
#############################
