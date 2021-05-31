#############################
# start of Parameter script
#############################
aw <- seq(0.8008,0.998001998001998,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1363 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp(3.247-10.25*sqrt(1-aw)+38.40*(sqrt(1-aw))^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1363 
#############################
plot(aw,mumax$mumax,
                          xlab='aw',
                          ylab='mu_max',main='Response surface mu_max for
Aspergillus oryzae in/on Basal medium
(gropin ID:1363)')
#############################
# End of Visualisation script
#############################
