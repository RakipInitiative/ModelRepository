#############################
# start of Parameter script
#############################
T <- seq(4.004,14.985014985015,length.out=21)
days <- seq(0,27.972027972028,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 493 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,days)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,days) {
   mumax <-0.247+0.297*days+0.363*T-0.0075*(days^2)-0.00832*(T^2)-0.000796*T*days

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['days']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 493 
#############################
persp(T,days,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='days',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Cheese _Milk_
(gropin ID:493)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
