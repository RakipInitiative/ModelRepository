#############################
# start of Parameter script
#############################
T <- seq(10.01,24.975024975025,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1262 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-13.65-131.9/T+530.6/T^2

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1262 
#############################
titleText <-'Response surface _mu_max for
Staphylococcus aureus in/on Beef raw _packaged_
(gropin ID:1262)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
