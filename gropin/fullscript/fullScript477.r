#############################
# start of Parameter script
#############################
pH <- seq(5.89410589410589,6.9069,length.out=21)
aw <- seq(0.959040959040959,0.993993,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 477 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-498.85+(-18.201)*pH+(-876.72)*aw+17.984*pH*aw+0.19199*(pH^2)+381.26*(aw^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 477 
#############################
persp(pH,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='pH',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:477)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
