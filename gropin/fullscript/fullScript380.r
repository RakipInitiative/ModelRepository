#############################
# start of Parameter script
#############################
T <- seq(13.4134,50.949050949051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 380 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(1.2*(T-51)*((T-13.4)^2))/((38.7-13.4)*((38.7-13.4)*(T-38.7)-(38.7-51)*(38.7+13.4-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 380 
#############################
titleText <-'Response surface _mu_max for
Bacillus subtilis in/on Nutrient broth
(gropin ID:380)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
