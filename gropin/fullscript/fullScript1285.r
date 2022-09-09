#############################
# start of Parameter script
#############################
T <- seq(1.001,6.99300699300699,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1285 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.39*T+2.81)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1285 
#############################
titleText <-'Response surface _mu_max for
Anaerobic Psychrotrophes in/on hake _Merluccius merluccius_ fillets
(gropin ID:1285)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################