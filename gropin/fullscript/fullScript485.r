#############################
# start of Parameter script
#############################
T <- seq(3.996003996004,12.012,length.out=21)
pH <- seq(3.7962037962038,5.005,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 485 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-15.5+4.6*pH+4.78*(pH^2)+4.9*T+0.7*(T^2)-5.1*(pH*T)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 485 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Egg salad
(gropin ID:485)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
