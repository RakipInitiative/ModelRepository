#############################
# start of Parameter script
#############################
pH <- seq(3.80619380619381,9.7097,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1198 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH) {
   mumax <-3.28*(pH-9.84)*((pH-3.78)^2)/((6.81-3.78)*((6.81-3.78)*(pH-6.81)-(6.81-9.84)*(6.81+3.78-2*pH)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1198 
#############################
plot(pH,mumax$mumax,
                          xlab='pH',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
