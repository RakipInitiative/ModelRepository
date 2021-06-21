#############################
# start of Parameter script
#############################
O2 <- seq(0.1001,5.99400599400599,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1075 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(O2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(O2) {
   mumax <-0.368*(1-(O2/6.61)^0.764)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['O2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1075 
#############################
titleText <-'Response surface _mu_max for
Clostridium perfringens in/on Food products _in modified atmosphere packaging_
(gropin ID:1075)'
plot(O2,responseSurface$'mumax',xlab='O2',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
