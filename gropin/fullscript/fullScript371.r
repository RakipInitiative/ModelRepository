#############################
# start of Parameter script
#############################
T <- seq(-7.8078,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 371 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.9*(10^-1)*(T-32.4)*((T+10.5)^2))/((25.3+10.5)*((25.3+10.5)*(T-25.3)-(25.3-32.4)*(25.3-10.5-2*T)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 371 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Achromobacter spp. in/on Ox muscle
(gropin ID:371)')
#############################
# End of Visualisation script
#############################
