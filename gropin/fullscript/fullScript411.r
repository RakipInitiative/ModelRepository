#############################
# start of Parameter script
#############################
T <- seq(36.8368,81.7182817182817,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 411 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.5*(10^-1)*(T-81.8)*((T-36.8)^2))/((70.9-36.8)*((70.9-36.8)*(T-70.9)-(70.9-81.8)*(70.9+36.8-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 411 
#############################
titleText <-'Response surface _mu_max for
Thermus aquaticus in/on Nutrient broth
(gropin ID:411)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
