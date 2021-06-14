#############################
# start of Parameter script
#############################
T <- seq(0,24.975024975025,length.out=21)
Phe <- seq(0,33.966033966034,length.out=21)
NaCl <- seq(0,7.99200799200799,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 286 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Phe,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,Phe,NaCl) {
   mumax <-(-0.0639+0.0137*T+0.0103*Phe+0.0257*NaCl-0.0001*T*Phe-0.0018*T*NaCl-0.0011*Phe*NaCl+(0.0004*(T^2))-(0.0002*(Phe^2))-(0.0011*(NaCl^2)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Phe'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 286 
#############################
argPar1 <- expand.grid(T,Phe)
argPar2 <- expand.grid(T,NaCl)
argPar3 <- expand.grid(Phe,NaCl)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],NaCl[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],Phe[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,Phe,z1,col = 'green',xlab='T',ylab='Phe',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,NaCl,z2,col = 'green',xlab='T',ylab='NaCl',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(Phe,NaCl,z3,col = 'green',xlab='Phe',ylab='NaCl',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface Sqr_mu_max for
Listeria monocytogenes in/on Cooked Salmon - treated with salt and liquid smoke
(gropin ID:286)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
