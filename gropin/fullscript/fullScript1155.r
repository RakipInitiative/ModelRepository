#############################
# start of Parameter script
#############################
T <- seq(15.8158,46.953046953047,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1155 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.088*(T-15.6)*(1-exp(0.066*(T-48.3)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1155 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Clostridium sporogenes in/on Ground beef _cooked_
(gropin ID:1155)')
#############################
# End of Visualisation script
#############################
