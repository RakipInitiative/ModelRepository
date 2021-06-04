#############################
# start of Parameter script
#############################
T <- seq(10.01,48.951048951049,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 269 
#############################
 
# constant coefficients for this model
A <- 0.00243
B <- 0.41
Tmin <- 13.5
Tmax <- 50.6
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(A*((T-Tmin)^2)*(1-exp(B*(T-Tmax))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 269 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Clostridium perfringens in/on Cooked cured pork
(gropin ID:269)')
#############################
# End of Visualisation script
#############################
