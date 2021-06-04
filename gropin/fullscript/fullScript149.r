#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
aw <- seq(0.961038961038961,0.986986,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 149 
#############################
 
# constant coefficients for this model
a <- 0.0141
awmin <- 0.9561
Tmin <- -8.1
 
variables <- data.frame(T,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw) {
   mumax <-(a*(aw-awmin)*((T-Tmin)^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 149 
#############################
persp(T,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='aw',zlab='mumax',main='Response surface mumax for
Lactobacillus sake in/on Cooked meat model _in modified BHI_
(gropin ID:149)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
