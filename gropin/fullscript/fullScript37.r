#############################
# start of Parameter script
#############################
T <- seq(6.99300699300699,25.025,length.out=21)
aw <- seq(0.899100899100899,0.985985,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 37 
#############################
 
# constant coefficients for this model
mopt <- 8.46
Tmin <- -1.39
Topt <- 21.4
Tmax <- 29.1
awmin <- 0.895
awopt <- 0.988
 
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
# start of Visualisation script Gropin ID 37 
#############################
persp(T,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Botrytis cinerea in/on Grape berry of "Red Globe" cultivars
(gropin ID:37)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
