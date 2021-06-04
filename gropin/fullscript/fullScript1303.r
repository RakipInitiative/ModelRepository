#############################
# start of Parameter script
#############################
T <- seq(15.015,39.96003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1303 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.0103*(T-6.53)*(1-exp(0.328*(T-44.97)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1303 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Listeria monocytogenes in/on Beef frankfurter
(gropin ID:1303)')
#############################
# End of Visualisation script
#############################
