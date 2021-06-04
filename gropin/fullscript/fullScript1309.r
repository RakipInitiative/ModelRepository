#############################
# start of Parameter script
#############################
T <- seq(15.015,39.96003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1309 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-1/(0.0179*(T-0.109)*(1-exp(0.2417*(T-46.03))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1309 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Listeria monocytogenes in/on Beef frankfurter
(gropin ID:1309)')
#############################
# End of Visualisation script
#############################
