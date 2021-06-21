#############################
# start of Parameter script
#############################
T <- seq(1.5015,39.7602397602398,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 393 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.4*(10^-1)*(T-39.8)*((T-1.5)^2))/((30.9-1.5)*((30.9-1.5)*(T-30.9)-(30.9-39.8)*(30.9+1.5-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 393 
#############################
titleText <-'Response surface _mu_max for
Gibberella fujikuroi in/on Nitrogen limited media
(gropin ID:393)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
