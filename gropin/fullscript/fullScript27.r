#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
aw <- seq(0.974974,0.991008991008991,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 27 
#############################
 
# constant coefficients for this model
c <- 0.143
awmin <- 0.974
Tmin <- -0.48499999999999999
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(1/(c*sqrt(aw-awmin)*(T-Tmin)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 27 
#############################
titleText <-'Response surface Sqr_mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:27)'
persp(T,aw,matrix(unlist(responseSurface$'Sqrmumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='Sqrmumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
