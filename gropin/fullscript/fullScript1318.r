#############################
# start of Parameter script
#############################
T <- seq(15.015,51.948051948052,length.out=21)
NaCl <- seq(0.5005,3.996003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1318 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,NaCl) {
   mumax <-7.45*((T-12.20)^2)*(1-exp(0.095*(T-54.47)))*( (1-NaCl*(5.2471+0.12206*NaCl)/1000)-0.9755)*(2-(1-NaCl*(5.2471+0.12206*NaCl)/1000)-0.9755)*(6.25-4.76)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1318 
#############################
persp(T,NaCl,matrix(unlist(responseSurface$'mumax'),nrow=21),col = 'green',xlab='T',ylab='NaCl',zlab='mumax',main='Response surface mumax for
Clostridium perfringens in/on Meat _bulk_
(gropin ID:1318)',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
#############################
# End of Visualisation script
#############################
