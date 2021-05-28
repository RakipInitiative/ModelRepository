#############################
# start of Parameter script
#############################
CO2 <- seq(24.975024975025,100.1,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 450 
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
# start of Visualisation script Gropin ID 450 
#############################
plot(CO2,mumax$mumax,
                          xlab='CO2',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
