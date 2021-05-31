#############################
# start of Parameter script
#############################
T <- seq(4.004,42.957042957043,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 501 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.975*((T-45.7)*((T-1.9)^2))/((36.4*(T-38.3)-(-7.4)*(36.4-2*T))*36.4)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 501 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Cantaloupe _fresh-cut_
(gropin ID:501)')
#############################
# End of Visualisation script
#############################
