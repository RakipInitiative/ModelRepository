#############################
# start of Parameter script
#############################
aw <- seq(0.85085,0.994005994005994,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1099 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp((2.183+6.445*sqrt(1-aw)-26.070*((sqrt(1-aw))^2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1099 
#############################
plot(aw,mumax$mumax,
                          xlab='aw',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
