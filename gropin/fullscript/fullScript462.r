#############################
# start of Parameter script
#############################
T <- seq(12.012,39.96003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 462 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-1.05*(10^-2)*(T-6.06)*(1-exp(8.09*(10^-1)*(T-43.71)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 462 
#############################
titleText <-'Response surface Sqr_mu_max for
Bysochlamys fulva in/on Papaya pulp
(gropin ID:462)'
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main=titleText)
#############################
# End of Visualisation script
#############################
