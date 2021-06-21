#############################
# start of Parameter script
#############################
T <- seq(23.023,36.963036963037,length.out=21)
Limonin <- seq(8.008,15.984015984016,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1080 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Limonin)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Limonin) {
   mumax <-(-191+ 14.04*T-0.227*(T^2)-0.029*(Limonin^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Limonin']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1080 
#############################
titleText <-'Response surface _mu_max for
Acinetobacter calcoaceticus in/on Orange juice _Sterilized_
(gropin ID:1080)'
persp(T,Limonin,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='Limonin',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
