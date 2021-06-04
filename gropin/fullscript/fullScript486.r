#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
pH <- seq(3.8038,4.995004995005,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 486 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-0.0571-0.0091*pH+0.0005*(pH^2)-0.0070*T+0.0006*(T^2)+0.0006*(pH*T)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 486 
#############################
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main='Response surface mumax for
Listeria monocytogenes in/on Pasta salad
(gropin ID:486)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
