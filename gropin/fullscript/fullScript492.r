#############################
# start of Parameter script
#############################
T <- seq(4.004,14.985014985015,length.out=21)
days <- seq(0,27.972027972028,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 492 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,days)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,days) {
   mumax <-0.89+0.081*days+0.11*T+0.000192*(days^2)-0.0034*(T^2)+0.0068*T*days

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['days']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 492 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Cheese _Milk_
(gropin ID:492)'
persp(T,days,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='days',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
