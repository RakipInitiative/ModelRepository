#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
pH <- seq(3.7037,5.0949050949051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 466 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-429.59-21.45*T-115.01*pH+4.987*T*pH-0.237*(T^2)+5.38*(pH^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 466 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Seafood salad
(gropin ID:466)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
