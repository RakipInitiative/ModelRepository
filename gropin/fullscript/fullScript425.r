#############################
# start of Parameter script
#############################
T <- seq(2.002,10.989010989011,length.out=21)
pH <- seq(5.2052,6.39360639360639,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 425 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-exp(-12.65+0.004234*(T^2)-0.3024*(pH^2)+0.01535*T*pH-0.004356*T+3.467*pH)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 425 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Brochothrix thermosphacta in/on Tryptic Soy Broth
(gropin ID:425)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
