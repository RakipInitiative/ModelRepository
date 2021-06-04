#############################
# start of Parameter script
#############################
T <- seq(2.7027,19.4805194805195,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 412 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.1*(10^-1)*(T-19.5)*((T-2.7)^2))/((15.6-2.7)*((15.6-2.7)*(T-15.6)-(15.6-19.5)*(15.6+2.7-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 412 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Vibrio marinus in/on Nutrient broth
(gropin ID:412)')
#############################
# End of Visualisation script
#############################
