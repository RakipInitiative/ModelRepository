#############################
# start of Parameter script
#############################
T <- seq(4.004,15.984015984016,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 491 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.061*(T-1.76)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 491 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Escherichia coli O157:H7 in/on Lettuce salad
(gropin ID:491)')
#############################
# End of Visualisation script
#############################
