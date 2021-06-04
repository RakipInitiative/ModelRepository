#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
pH <- seq(3.7037,5.0949050949051,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 467 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH) {
   mumax <-(-0.07323+0.00168*T+0.03188*pH-0.000038*T*pH+0.000017*(T^2)-0.00333*(pH^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 467 
#############################
persp(T,pH,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='pH',zlab='mumax',main='Response surface mumax for
Listeria monocytogenes in/on Seafood salad
(gropin ID:467)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
