#############################
# start of Parameter script
#############################
pH <- seq(3.996003996004,7.007,length.out=21)
aw <- seq(0.963036963036963,0.992992,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1332 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-2.14*((pH-3.84)*(pH-14.1)/((pH-3.84)*(pH-14.1)-((pH-6.47)^2)))*(((aw-0.939)/(0.992-0.939))^2)

return(mumax=mumax)
} 

# output parameters
mumax <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(mumax) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1332 
#############################
persp(pH,aw,matrix(unlist(mumax$mumax),nrow=21),col = 'green',xlab='pH',ylab='aw',zlab='mu_max',main='Response surface mu_max for
Salmonella Enterica in/on TSB
(gropin ID:1332)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
