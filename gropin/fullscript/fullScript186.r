#############################
# start of Parameter script
#############################
pH <- seq(5.5055,6.49350649350649,length.out=21)
T <- seq(4.004,9.99000999000999,length.out=21)
CO2 <- seq(0.1001,0.899100899100899,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 186 
#############################
 
# constant coefficients for this model
a1 <- 2.9465
a2 <- -0.3604
a3 <- -0.47420000000000001
a4 <- 5.6668
a5 <- 0.03049
a6 <- -0.6344
a7 <- -0.03586
a8 <- 0.0076
a9 <- -0.2589
 
variables <- data.frame(pH,T,CO2)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,T,CO2) {
   mumax <-a1+(a2*pH)+(a3*T)+(a4*CO2)+(a5*pH*T)+(a6*pH*CO2)+(a7*T*CO2)+(a8*(T^2))+(a9*(CO2^2))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['T'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 186 
#############################
argPar1 <- expand.grid(pH,T)
argPar2 <- expand.grid(pH,CO2)
argPar3 <- expand.grid(T,CO2)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],CO2[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],T[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(pH[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(pH,T,z1,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,CO2,z2,col = 'green',xlab='pH',ylab='CO2',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,CO2,z3,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:186)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
