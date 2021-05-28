#############################
# start of Parameter script
#############################
pH <- seq(6.34365634365634,6.8068,length.out=21)
NO2 <- seq(0,315.315,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 480 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,NO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,NO2) {
   mumax <-(-0.12635+0.027170*pH+0.031746*NO2+(-0.008778)*pH*NO2+(-0.005576)*(pH^2)+0.024220*(NO2^2))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['NO2']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 480 
#############################
persp(pH,NO2,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='pH',ylab='NO2',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:480)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
