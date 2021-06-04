#############################
# start of Parameter script
#############################
T <- seq(6.5065,29.6703296703297,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 413 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.1*(10^-1)*(T-29.7)*((T-6.5)^2))/((23.8-6.5)*((23.8-6.5)*(T-23.8)-(23.8-29.7)*(23.8+6.5-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 413 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Vibrio marinus in/on Nutrient broth
(gropin ID:413)')
#############################
# End of Visualisation script
#############################
