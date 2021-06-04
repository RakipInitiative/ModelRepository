#############################
# start of Parameter script
#############################
T <- seq(45.045,58.9410589410589,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1101 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-sqrt((1/10.3)* ((T-64.2)*((T-38.2)^2))/((53.6-38.2)* (((53.6-38.2)*(T-53.6)-(53.6-64.2)*(53.6+38.2-2*T)))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1101 
#############################
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main='Response surface Sqrmumax for
Geobacillus stearothermophilus in/on Thermally processed foods
(gropin ID:1101)')
#############################
# End of Visualisation script
#############################
