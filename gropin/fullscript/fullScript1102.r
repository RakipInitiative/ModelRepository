#############################
# start of Parameter script
#############################
T <- seq(16.016,27.972027972028,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1102 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.143*(T-1.65)*(1-exp(0.145*(T-46.65)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1102 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Bysochlamys fulva in/on Papaya pulp
(gropin ID:1102)')
#############################
# End of Visualisation script
#############################
