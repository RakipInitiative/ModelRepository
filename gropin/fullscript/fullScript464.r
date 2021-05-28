#############################
# start of Parameter script
#############################
T <- seq(3.996003996004,12.012,length.out=21)
pH <- seq(3.6963036963037,5.1051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 464 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-620.83+2.916*T-186.5*pH+3.841*T*pH-1.463*(T^2)+9.879*(pH^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 464 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Seafood salad
(gropin ID:464)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
