#############################
# start of Parameter script
#############################
T <- seq(3.996003996004,12.012,length.out=21)
aw <- seq(0.973026973026973,0.992992,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 23 
#############################
 
# constant coefficients for this model
a <- 0.032
awmin <- 0.9718
Tmin <- -5.25
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(a*(aw-awmin)*((T-Tmin)^2))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 23 
#############################
persp(T,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:23)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
