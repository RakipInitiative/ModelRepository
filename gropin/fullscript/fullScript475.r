#############################
# start of Parameter script
#############################
pH <- seq(5.89410589410589,6.9069,length.out=21)
aw <- seq(0.959040959040959,0.993993,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 475 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-(-19.684+0.5085*pH+36.254*aw-0.4970*pH*aw+0.0046939*(pH^2)-16.581*(aw^2))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 475 
#############################
persp(pH,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='pH',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:475)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
