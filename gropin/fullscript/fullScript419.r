#############################
# start of Parameter script
#############################
T <- seq(1.998001998002,11.011,length.out=21)
pH <- seq(5.1948051948052,6.4064,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 419 
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
# start of Visualisation script Gropin ID 419 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Psychrobacter immobilis in/on Tryptic Soy Broth
(gropin ID:419)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
