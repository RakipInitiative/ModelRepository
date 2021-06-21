#############################
# start of Parameter script
#############################
T <- seq(0,11.988011988012,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 428 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(0.0306*(T+7.85))^2

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 428 
#############################
titleText <-'Response surface _mu_max for
Pseudomonas fragi in/on Milk _dried, dehydrated non fat_
(gropin ID:428)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
