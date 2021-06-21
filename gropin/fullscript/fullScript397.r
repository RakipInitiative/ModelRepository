#############################
# start of Parameter script
#############################
T <- seq(-0.9009,37.1628371628372,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 397 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.3*(10^-3)*(T-37.2)*((T+0.9)^2))/((30.5+0.9)*((30.5+0.9)*(T-30.5)-(30.5-37.2)*(30.5-0.9-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 397 
#############################
titleText <-'Response surface _mu_max for
Moraxella spp. in/on Nutrient broth
(gropin ID:397)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
