#############################
# start of Parameter script
#############################
CO2 <- seq(0,14.985014985015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1276 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(CO2) {
   mumax <-0.00204*(31.5-CO2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['CO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1276 
#############################
plot(CO2,mumax$mumax,
                          xlab='CO2',
                          ylab='mu_max',main='Response surface mu_max for
Listeria innocua in/on Lettuce_fresh-cut butterhead_
(gropin ID:1276)')
#############################
# End of Visualisation script
#############################
