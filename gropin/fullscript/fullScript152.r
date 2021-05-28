#############################
# start of Parameter script
#############################
T <- seq(3.996003996004,12.012,length.out=21)
aw <- seq(0.962962,0.985014985014985,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 152 
#############################
 
# constant coefficients for this model
c <- 0.012
awmin <- 0.9469
Tmin <- -2.31
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(1/(c*(aw-awmin)*((T-Tmin)^2)))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 152 
#############################
persp(T,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Lactobacillus sake in/on Cooked meat model _in modified BHI_
(gropin ID:152)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
