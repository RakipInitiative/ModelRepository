#############################
# start of Parameter script
#############################
T <- seq(-5.2052,33.3666333666334,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 375 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(7.5*(10^-2)*(T-33.4)*((T+5.2)^2))/((25.5+5.2)*((25.5+5.2)*(T-25.5)-(25.5-33.4)*(25.5-5.2-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 375 
#############################
titleText <-'Response surface _mu_max for
Alteromonas spp. in/on Nutrient broth
(gropin ID:375)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
