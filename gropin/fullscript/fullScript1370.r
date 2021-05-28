#############################
# start of Parameter script
#############################
bw <- seq(0.031968031968032,0.4476472,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1370 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(bw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(bw) {
   mumax <-(-0.4344+6.904*bw-29.72*bw^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['bw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1370 
#############################
plot(bw,mumax$mumax,
                          xlab='bw',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
