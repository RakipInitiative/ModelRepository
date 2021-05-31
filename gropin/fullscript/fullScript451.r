#############################
# start of Parameter script
#############################
CO2 <- seq(25.025,99.9000999000999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 451 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(CO2) {
   mumax <-sqrt(118*(CO2+0.007))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['CO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 451 
#############################
plot(CO2,mumax$mumax,
                          xlab='CO2',
                          ylab='mu_max',main='Response surface mu_max for
Listeria innocua in/on nutrient agar surface
(gropin ID:451)')
#############################
# End of Visualisation script
#############################
