#############################
# start of Parameter script
#############################
T <- seq(22.977022977023,37.037,length.out=21)
Limonin <- seq(7.99200799200799,16.016,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1077 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Limonin)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,Limonin) {
   mumax <-7.26-0.386*T+0.006*(T^2)-0.257*Limonin+0.008*T*Limonin

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Limonin']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1077 
#############################
persp(T,Limonin,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='Limonin',zlab='mu_max',main='Response surface mu_max for
Acinetobacter calcoaceticus in/on Orange juice _raw_
(gropin ID:1077)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
