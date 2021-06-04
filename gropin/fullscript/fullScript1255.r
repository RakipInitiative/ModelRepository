#############################
# start of Parameter script
#############################
T <- seq(5.005,36.963036963037,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1255 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-18.90+(-0.8647*T)+(0.01029*T^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1255 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Salmonella spp. in/on Egg yolk
(gropin ID:1255)')
#############################
# End of Visualisation script
#############################
