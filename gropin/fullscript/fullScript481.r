#############################
# start of Parameter script
#############################
pH <- seq(5.87587,6.88311688311688,length.out=21)
aw <- seq(0.96096,0.992007992007992,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 481 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-(-1892+81.006*pH+3270*aw+(-79.839)*pH*aw+(-2.3715)*(pH^2)+(-1382.3)*(aw^2))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 481 
#############################
persp(pH,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='pH',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:481)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
