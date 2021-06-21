#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
aw <- seq(0.974974,0.991008991008991,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 25 
#############################
 
# constant coefficients for this model
Im <- 89.6
m1 <- -0.8
m3 <- -181
m4 <- 0.00013
m6 <- 92.1
m8 <- 0.82
 
variables <- data.frame(T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(Im+(m1*T)+(m3*aw)+(m4*(T^2))+(m6*(aw^2))+(m8*T*aw))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 25 
#############################
titleText <-'Response surface _mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:25)'
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main=titleText,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
