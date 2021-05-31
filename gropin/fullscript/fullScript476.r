#############################
# start of Parameter script
#############################
pH <- seq(5.9059,6.89310689310689,length.out=21)
aw <- seq(0.96096,0.992007992007992,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 476 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-(-112.09+(-27.702)*pH+467.55*aw+6.9724*pH*aw+1.5135*(pH^2)+(-282.75)*(aw^2))

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 476 
#############################
persp(pH,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='pH',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Listeria monocytogenes in/on Meat _Cooked_
(gropin ID:476)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
