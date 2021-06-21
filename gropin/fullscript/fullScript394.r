#############################
# start of Parameter script
#############################
T <- seq(-7.1071,25.4745254745255,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 394 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(2.9*(10^-1)*(T-25.5)*((T+7.1)^2))/((21.5+7.1)*((21.5+7.1)*(T-21.5)-(21.5-25.5)*(21.5-7.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 394 
#############################
titleText <-'Response surface _mu_max for
Micrococcus cryophilus in/on Various
(gropin ID:394)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
