#############################
# start of Parameter script
#############################
T <- seq(10.01,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1109 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.0370*(T-2.94)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1109 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Bysochlamys fulva in/on Apple juice _solidified_
(gropin ID:1109)')
#############################
# End of Visualisation script
#############################
