#############################
# start of Parameter script
#############################
T <- seq(-2.9029,28.971028971029,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 384 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3*(10^-1)*(T-29)*((T+2.9)^2))/((25.4+2.9)*((25.4+2.9)*(T-25.4)-(25.4-29)*(25.4-2.9-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 384 
#############################
titleText <-'Response surface _mu_max for
Clostridium botulinum in/on Sealed cultures
(gropin ID:384)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
