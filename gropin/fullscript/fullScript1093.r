#############################
# start of Parameter script
#############################
aw <- seq(0.85085,0.994005994005994,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1093 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-0.204+0.389*sqrt(1-aw)+7.927*((sqrt(1-aw))^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1093 
#############################
plot(aw,mumax$mumax,
                          xlab='aw',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
