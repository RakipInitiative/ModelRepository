#############################
# start of Parameter script
#############################
aw <- seq(0.799200799200799,0.999999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1365 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp(-0.7385+6.720*sqrt(1-aw)-27.08*(sqrt(1-aw))^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1365 
#############################
plot(aw,mumax$mumax,
                          xlab='aw',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
