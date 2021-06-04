#############################
# start of Parameter script
#############################
T <- seq(15.015,39.96003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1305 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.000199*((T+6.05)^2)*(1-exp(0.163*(T-46.51)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1305 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Listeria monocytogenes in/on Beef frankfurter
(gropin ID:1305)')
#############################
# End of Visualisation script
#############################
