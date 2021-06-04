#############################
# start of Parameter script
#############################
T <- seq(8.2082,46.7532467532468,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1172 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.21*(T-46.8)*((T-8.23)^2)/(((37.6-8.23)*(T-37.6)-(37.6-46.8)*(37.6+8.23-2*T))*(37.6-8.23))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1172 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Bacillus cereus in/on Rice _cooked_
(gropin ID:1172)')
#############################
# End of Visualisation script
#############################
