#############################
# start of Parameter script
#############################
T <- seq(5.005,24.975024975025,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 258 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.033*(T-4.96)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 258 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max',main='Response surface mu_max for
Salmonella spp. in/on Lettuce salad
(gropin ID:258)')
#############################
# End of Visualisation script
#############################
