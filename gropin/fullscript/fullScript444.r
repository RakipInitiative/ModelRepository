#############################
# start of Parameter script
#############################
T <- seq(2.002,15.984015984016,length.out=21)
pH <- seq(5.2052,6.79320679320679,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 444 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(-3.929+0.185*T+0.2142*pH-0.003181*(T^2)-0.008647*(pH^2)-0.001372*T*pH)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 444 
#############################
persp(T,pH,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mu_max',main='Response surface mu_max for
Pseudomonas spp. in/on Tryptic Soy Broth
(gropin ID:444)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
