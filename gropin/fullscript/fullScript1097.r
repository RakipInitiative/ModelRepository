#############################
# start of Parameter script
#############################
aw <- seq(0.849150849150849,0.995995,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1097 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw) {
   mumax <-(2.712+5.975*sqrt(1-aw)-30.204*((sqrt(1-aw))^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1097 
#############################
titleText <-'Response surface _mu_max for
Bysochlamys fulva in/on Fruit based products
(gropin ID:1097)'
plot(aw,responseSurface$'mumax',xlab='aw',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
