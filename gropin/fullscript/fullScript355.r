#############################
# start of Parameter script
#############################
T <- seq(4.004,37.962037962038,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 355 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-0.0356*(T-2.33)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 355 
#############################
titleText <-'Response surface Sqr_mu_max for
Salmonella spp. in/on Pork _fresh_
(gropin ID:355)'
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main=titleText)
#############################
# End of Visualisation script
#############################
