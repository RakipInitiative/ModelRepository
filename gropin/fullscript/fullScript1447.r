#############################
# start of Parameter script
#############################
T <- seq(28.028,47.952047952048,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1447 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1/(0.0009765*(T-3.217)^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1447 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Salmonella Typhimurium in/on Chicken _cooked_
(gropin ID:1447)')
#############################
# End of Visualisation script
#############################
