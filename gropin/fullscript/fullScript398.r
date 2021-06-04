#############################
# start of Parameter script
#############################
T <- seq(-5.7057,30.6693306693307,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 398 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.3*(10^-1)*(T-30.7)*((T+5.7)^2))/((28.4+5.7)*((28.4+5.7)*(T-28.4)-(28.4-30.7)*(28.4-5.7-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 398 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Mycotorula spp. in/on Ox muscle
(gropin ID:398)')
#############################
# End of Visualisation script
#############################
