#############################
# start of Parameter script
#############################
T <- seq(6.006,44.955044955045,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 353 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(-0.366+3.003*exp(-((log(2)/(log(0.296)^2))*log((((T-42.330)*((0.296^2)-1))/(15.653*0.296))+1)^2)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 353 
#############################
titleText <-'Response surface _mu_max for
Escherichia coli O157:H7 in/on Brain Heart Infusion broth
(gropin ID:353)'
plot(T,responseSurface$'mumax',xlab='T',
                          ylab='mumax',main=titleText)
#############################
# End of Visualisation script
#############################
