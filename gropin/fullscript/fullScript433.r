#############################
# start of Parameter script
#############################
T <- seq(7.007,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 433 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(0.0513*T-0.71)^2

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 433 
#############################
titleText <-'Response surface _mu_max for
Escherichia coli O157:H7 in/on Beef carcass
(gropin ID:433)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
