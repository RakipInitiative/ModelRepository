#############################
# start of Parameter script
#############################
T <- seq(-11.3113,31.968031968032,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 381 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(5.4*(10^-2)*(T-32)*((T+11.3)^2))/((24.9+11.3)*((24.9+11.3)*(T-24.9)-(24.9-32)*(24.9-11.3-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 381 
#############################
titleText <-'Response surface _mu_max for
Cytophaga johnsonae in/on Nutrient broth
(gropin ID:381)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
