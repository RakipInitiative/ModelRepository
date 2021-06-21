#############################
# start of Parameter script
#############################
T <- seq(-7.5075,33.0669330669331,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 382 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(6.2*(10^-2)*(T-33.1)*((T+7.5)^2))/((27.8+7.5)*((27.8+7.5)*(T-27.8)-(27.8-33.1)*(27.8-7.5-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 382 
#############################
titleText <-'Response surface _mu_max for
Cytophaga johnsonae in/on Nutrient broth
(gropin ID:382)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
