#############################
# start of Parameter script
#############################
T <- seq(19.8198,59.1408591408591,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 377 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.6*(T-59.2)*((T-19.8)^2))/((50.3-19.8)*((50.3-19.8)*(T-50.3)-(50.3-59.2)*(50.3+19.8-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 377 
#############################
titleText <-'Response surface _mu_max for
Bacillus megaterium in/on Nutrient broth
(gropin ID:377)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
