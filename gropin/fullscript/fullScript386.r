#############################
# start of Parameter script
#############################
T <- seq(13.013,46.0539460539461,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 386 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1*(T-46.1)*((T-13)^2))/((37.9-13)*((37.9-13)*(T-37.9)-(37.9-46.1)*(37.9+13-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 386 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Cellulomonas spp. in/on Oxoid tryptone soya broth
(gropin ID:386)')
#############################
# End of Visualisation script
#############################
