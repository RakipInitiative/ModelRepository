#############################
# start of Parameter script
#############################
T <- seq(13.15314,42.6173826173826,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1154 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T) {
   mumax <-exp(2.21)*(0.633*(T-44.3)*((T-11.5)^2)/(((36.4-11.5)*(T-36.4)-(36.4-44.3)*(36.4+11.5-2*T))*(36.4-11.5)))^(-0.786)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1154 
#############################
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main='Response surface mumax for
Clostridium botulinum in/on Ground beef _cooked_
(gropin ID:1154)')
#############################
# End of Visualisation script
#############################
