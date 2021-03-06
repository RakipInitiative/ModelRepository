#############################
# start of Parameter script
#############################
T <- seq(2.002,29.97002997003,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 38 
#############################
 
# constant coefficients for this model
Tmin <- -1.39
Topt <- 21.4
Tmax <- 29.1
 
variables <- data.frame(T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T) {
   mumax <-(12.9^0.5)*(((T-Tmax)*(T-Tmin)^2)/((Topt-Tmin)*(((Topt-Tmin)*(T-Topt))-((Topt-Tmax)*(Topt+Tmin-2*T)))))^0.5

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 38 
#############################
titleText <-'Response surface Sqr_mu_max for
Botrytis cinerea in/on Potato Dextrose Agar
(gropin ID:38)'
plot(T,responseSurface$'Sqrmumax',xlab='T',
                          ylab='Sqrmumax',main=titleText)
#############################
# End of Visualisation script
#############################
