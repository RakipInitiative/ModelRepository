#############################
# start of Parameter script
#############################
T <- seq(-4.3956043956044,32.5325,length.out=21)
aw <- seq(0.855144855144855,0.982982,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 170 
#############################
 
# constant coefficients for this model
mopt <- 4.19
Tmin <- -4.4
Topt <- 23.9
Tmax <- 32.5
awmin <- 0.856
awopt <- 0.981
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(mopt*(((T-Tmax)*((T-Tmin)^2))/((Topt-Tmin)*((Topt-Tmin)*(T-Topt)-(Topt-Tmax)*(Topt+Tmin-2*T))))*(((aw-1)*(aw-awmin)^2)/((awopt-awmin)*((awopt-awmin)*(aw-awopt)-(awopt-1)*(awopt+awmin-2*aw)))))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 170 
#############################
persp(T,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Penicillium expansum in/on Grape berry of "Red Globe" cultivars
(gropin ID:170)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
