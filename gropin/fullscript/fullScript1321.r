#############################
# start of Parameter script
#############################
T <- seq(4.004,14.985014985015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1321 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.024*(T+6.2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1321 
#############################
titleText <-'Response surface Sqr_mu_max for
Lactic acid bacteria in/on Chicken breast fillets
(gropin ID:1321)'
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main=titleText)
#############################
# End of Visualisation script
#############################
