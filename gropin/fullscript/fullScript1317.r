#############################
# start of Parameter script
#############################
T <- seq(15.015,51.948051948052,length.out=21)
NaCl <- seq(0.5005,3.996003996004,length.out=21)
pH <- seq(5.005,6.24375624375624,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1317 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,NaCl,pH) {
   mumax <-7.45*((T-12.20)^2)*(1-exp(0.095*(T-54.47)))*( (1-NaCl*(5.2471+0.12206*NaCl)/1000)-0.9755)*(2-(1-NaCl*(5.2471+0.12206*NaCl)/1000)-0.9755)*(pH-4.76)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaCl'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1317 
#############################
argPar1 <- expand.grid(T,NaCl)
argPar2 <- expand.grid(T,pH)
argPar3 <- expand.grid(NaCl,pH)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],pH[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],NaCl[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,NaCl,z1,col = 'green',xlab='T',ylab='NaCl',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,pH,z2,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(NaCl,pH,z3,col = 'green',xlab='NaCl',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Clostridium perfringens in/on Meat _bulk_
(gropin ID:1317)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
