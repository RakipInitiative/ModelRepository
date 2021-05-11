#############################
# start of Parameter script
#############################
T <- seq(4,12,length.out=21)
aw <- seq(0.974,0.992,length.out=21)
a <- 0.032
awmin <- 0.9718
Tmin <- -5.25
#############################
# end of Parameter script
#############################
#############################
# start of Model script
#############################
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
response_surface <- function(T,aw) {
   mumax <-matrix(unlist((a*(aw-awmin)*((T-Tmin)^2)),nrow=21))
return(mumax=mumax)
} 
mumax <- response_surface(argumentsPar['T'],argumentsPar['aw'])
#############################
# End of Model script
#############################
#############################
# start of Visualisation script
#############################
persp(T,aw,mumax,col = 'green',xlab='T',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:23)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
