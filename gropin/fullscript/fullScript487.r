#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
pH <- seq(3.8038,4.995004995005,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 487 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-1697.6-251.4*pH+6*(pH^2)-180.4*T+5.8*(T^2)+13*(T*pH)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 487 
#############################
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main='Response surface mumax for
Listeria monocytogenes in/on Pasta salad
(gropin ID:487)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
