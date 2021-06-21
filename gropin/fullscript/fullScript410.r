#############################
# start of Parameter script
#############################
T <- seq(-2.1021,31.2687312687313,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 410 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.3*(T-31.3)*((T+2.1)^2))/((25.8+2.1)*((25.8+2.1)*(T-25.8)-(25.8-31.3)*(25.8-2.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 410 
#############################
titleText <-'Response surface _mu_max for
Serratia marcescens in/on Nutrient broth
(gropin ID:410)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
