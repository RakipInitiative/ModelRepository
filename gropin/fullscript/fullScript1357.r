#############################
# start of Parameter script
#############################
aw <- seq(0.8008,0.998001998001998,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1357 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw) {
   mumax <-exp(-0.8461+11.960*sqrt(1-aw)-39.51*(sqrt(1-aw))^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1357 
#############################
titleText <-'Response surface _mu_max for
Aspergillus nomius in/on Basal medium
(gropin ID:1357)'
plot(aw,responseSurface$'mumax',xlab='aw',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
