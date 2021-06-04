#############################
# start of Parameter script
#############################
pH <- seq(3.3033,7.79220779220779,length.out=21)
aw <- seq(0.894894,0.996003996003996,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1331 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw) {
   mumax <-((pH-9.71)*(pH-3.58)/((6.02-3.58)*(pH-6.02)-(6.02-9.71)*(3.58-pH)))*((aw-1)*((aw-1)^2)/((0.994-0.894)*((0.994-0.894)*(aw-0.994)-(0.994-1)*(0.994+0.894-2*aw))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1331 
#############################
persp(pH,aw,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='pH',ylab='aw',zlab='mumax',main='Response surface mumax for
Salmonella ser.Newport in/on TSB
(gropin ID:1331)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
