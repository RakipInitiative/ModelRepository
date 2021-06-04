#############################
# start of Parameter script
#############################
T <- seq(28.028,47.952047952048,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1446 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-exp(-0.6199+(42.64/(T))-(57.63/(T)^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1446 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Salmonella Typhimurium in/on Chicken _cooked_
(gropin ID:1446)')
#############################
# End of Visualisation script
#############################
