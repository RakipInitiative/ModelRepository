#############################
# start of Parameter script
#############################
T <- seq(8.44844,46.3536463536464,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 284 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.35*((T-8.44)^2)*(T-46.4))/((41.5-8.44)*((41.5-8.44)*(T-41.5)-(41.5-46.4)*(8.44+41.5-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 284 
#############################
titleText <-'Response surface _mu_max for
Escherichia coli in/on BHI
(gropin ID:284)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
