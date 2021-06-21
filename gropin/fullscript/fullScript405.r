#############################
# start of Parameter script
#############################
T <- seq(-10.1101,33.966033966034,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 405 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(8*(10^-1)*(T-34)*((T+10.1)^2))/((30.8+10.1)*((30.8+10.1)*(T-30.8)-(30.8-34)*(30.8-10.1-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 405 
#############################
titleText <-'Response surface _mu_max for
Pseudomonas psychrophiles in/on Tryptic Soy Broth
(gropin ID:405)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
