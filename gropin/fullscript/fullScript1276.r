#############################
# start of Parameter script
#############################
CO2 <- seq(0,14.985014985015,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1276 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(CO2) {
   mumax <-0.00204*(31.5-CO2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1276 
#############################
titleText <-'Response surface _mu_max for
Listeria innocua in/on Lettuce_fresh-cut butterhead_
(gropin ID:1276)'
plot(CO2,responseSurface$'mumax',xlab='CO2',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
