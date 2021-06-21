#############################
# start of Parameter script
#############################
T <- seq(1.1011,36.7632367632368,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 415 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(4.4*(10^-1)*(T-36.8)*((T-1.1)^2))/((30.8-1.1)*((30.8-1.1)*(T-30.8)-(30.8-36.8)*(30.8+1.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 415 
#############################
titleText <-'Response surface _mu_max for
Xanthomonas pruni in/on Nutrient broth
(gropin ID:415)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
