#############################
# start of Parameter script
#############################
aw <- seq(0.849150849150849,0.995995,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1095 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw) {
   mumax <-(2.290+4.119*sqrt(1-aw)-17.589*((sqrt(1-aw))^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1095 
#############################
titleText <-'Response surface _mu_max for
Neosartorya fischeri in/on Fruit based products
(gropin ID:1095)'
plot(aw,responseSurface$'mumax',xlab='aw',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
