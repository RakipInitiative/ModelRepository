#############################
# start of Parameter script
#############################
T <- seq(0,33.966033966034,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 440 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-9400/(8.3144261*(T+273)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 440 
#############################
titleText <-'Response surface ln_mu_max for
Pseudomonas fluorescens in/on Tryptic Soy Broth
(gropin ID:440)'
plot(T,responseSurface$'lnmumax',xlab='T',
                          ylab='lnmumax',main=titleText)
#############################
# End of Visualisation script
#############################
