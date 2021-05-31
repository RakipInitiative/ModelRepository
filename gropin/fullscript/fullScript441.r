#############################
# start of Parameter script
#############################
T <- seq(8.008,45.954045954046,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 441 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-14000/(8.3144261*(T+273)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 441 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Escherichia coli O157:H7 in/on Tryptic Soy Broth
(gropin ID:441)')
#############################
# End of Visualisation script
#############################
