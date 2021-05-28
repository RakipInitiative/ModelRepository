#############################
# start of Parameter script
#############################
T <- seq(-4.3956043956044,32.5325,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 171 
#############################
 
# constant coefficients for this model
Tmin <- -4.4
Topt <- 23.9
Tmax <- 32.5
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-((2.4^0.5)*(((T-Tmax)*(T-Tmin)^2)/((Topt-Tmin)*(((Topt-Tmin)*(T-Topt))-((Topt-Tmax)*(Topt+Tmin-2*T)))))^0.5)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 171 
#############################
plot(T,mumax$mumax,
                          xlab='T',
                          ylab='mu_max')
#############################
# End of Visualisation script
#############################
