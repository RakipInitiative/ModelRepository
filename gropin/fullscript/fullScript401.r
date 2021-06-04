#############################
# start of Parameter script
#############################
T <- seq(9.8098,45.3546453546454,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 401 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.2*(T-45.4)*((T-9.8)^2))/((38.6-9.8)*((38.6-9.8)*(T-38.6)-(38.6-45.4)*(38.6+9.8-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 401 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Pseudomonas fluorescens in/on Nutrient broth
(gropin ID:401)')
#############################
# End of Visualisation script
#############################
