#############################
# start of Parameter script
#############################
T <- seq(0.9009,37.8621378621379,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 395 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(3.8*(10^-1)*(T-37.9)*((T-0.9)^2))/((30.9-0.9)*((30.9-0.9)*(T-30.9)-(30.9-37.9)*(30.9+0.9-2*T)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 395 
#############################
titleText <-'Response surface _mu_max for
Micrococcus cryophilus in/on Various
(gropin ID:395)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
