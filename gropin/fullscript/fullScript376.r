#############################
# start of Parameter script
#############################
T <- seq(12.1121,62.8371628371628,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 376 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1*(T-62.9)*((T-12.1)^2))/((53.8-12.1)*((53.8-12.1)*(T-53.8)-(53.8-62.9)*(53.8+12.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 376 
#############################
titleText <-'Response surface _mu_max for
Bacillus coagulans in/on Nutrient broth
(gropin ID:376)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
