#############################
# start of Parameter script
#############################
T <- seq(4.004,15.984015984016,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1314 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-1/(0.004*(T+27.5))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1314 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Pseudomonas spp. in/on Oyster mushrooms _Pleurotus ostreatus_
(gropin ID:1314)')
#############################
# End of Visualisation script
#############################
