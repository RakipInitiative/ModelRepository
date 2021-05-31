#############################
# start of Parameter script
#############################
aw <- seq(0.8008,0.998001998001998,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1383 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp(2.712-3.740*sqrt(1-aw)+22.48*(sqrt(1-aw))^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1383 
#############################
plot(aw,mumax$mumax,
                          xlab='aw',
                          ylab='mu_max',main='Response surface mu_max for
Aspergillus parasiticus in/on Basal medium
(gropin ID:1383)')
#############################
# End of Visualisation script
#############################
