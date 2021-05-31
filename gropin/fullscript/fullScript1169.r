#############################
# start of Parameter script
#############################
T <- seq(10.01,44.955044955045,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1169 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.00028*((T+0.67)^2)*(1-exp(0.57*(T-47.36)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1169 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Salmonella spp. in/on Chicken
(gropin ID:1169)')
#############################
# End of Visualisation script
#############################
