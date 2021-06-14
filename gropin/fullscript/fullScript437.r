#############################
# start of Parameter script
#############################
T <- seq(4.004,29.97002997003,length.out=21)
pH <- seq(6.006,7.99200799200799,length.out=21)
NaCl <- seq(0,4.995004995005,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 437 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-(-0.2554-9499.95*(1/(T+273))-7.22*NaCl+8.85*pH-0.6114*(pH^2)+1615.8*(1/(T+273))*NaCl+0.1336*NaCl*pH)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 437 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,NaCl)
argPar3 <- expand.grid(pH,NaCl)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],NaCl[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,NaCl,z2,col = 'green',xlab='T',ylab='NaCl',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,NaCl,z3,col = 'green',xlab='pH',ylab='NaCl',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Pseudomonas marginalis in/on Pectin-NH4Cl-MgSO4
(gropin ID:437)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
