#############################
# start of Parameter script
#############################
T <- seq(1.4014,35.0649350649351,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 372 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.3*(10^-3)*(T-35.1)*((T-1.4)^2))/((28.4-1.4)*((28.4-1.4)*(T-28.4)-(28.4-35.1)*(28.4+1.4-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 372 
#############################
titleText <-'Response surface _mu_max for
Acinetobacter spp. in/on Nutrient broth
(gropin ID:372)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
