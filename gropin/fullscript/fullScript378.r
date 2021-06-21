#############################
# start of Parameter script
#############################
T <- seq(30.8308,65.4345654345654,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 378 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.8*(10^-2)*(T-65.5)*((T-30.8)^2))/((57.2-30.8)*((57.2-30.8)*(T-57.2)-(57.2-65.5)*(57.2+30.8-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 378 
#############################
titleText <-'Response surface _mu_max for
Bacillus stearothermophilus in/on Nutrient broth
(gropin ID:378)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
