#############################
# start of Parameter script
#############################
T <- seq(0,19.98001998002,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 249 
#############################
 
# constant coefficients for this model
a <- 46.6162
Ea <- 109.911
R <- 8.3140000000000002E-3
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-2.302585*exp(a-(Ea/R)*(1/(T+273)))/24

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 249 
#############################
titleText <-'Response surface _mu_max for
Lactic acid bacteria in/on Cooked cured meat products
(gropin ID:249)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
