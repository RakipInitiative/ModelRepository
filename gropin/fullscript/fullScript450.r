#############################
# start of Parameter script
#############################
CO2 <- seq(25.025,99.9000999000999,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 450 
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
# start of Visualisation script Gropin ID 450 
#############################
titleText <-'Response surface _mu_max for
Listeria innocua in/on nutrient agar surface
(gropin ID:450)'
plot(CO2,responseSurface$'mumax',xlab='CO2',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
