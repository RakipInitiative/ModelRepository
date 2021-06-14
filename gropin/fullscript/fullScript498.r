#############################
# start of Parameter script
#############################
T <- seq(0,34.965034965035,length.out=21)
pH <- seq(4.6046,7.39260739260739,length.out=21)
NaCl <- seq(0,79.9200799200799,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 498 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-0.46*(((T-27.19)*((T+4.8)^2))/(28.19*(28.19*(T-23.39)-(-3.8)*(18.59-2*T))))*(1/6.6)*((pH-4.79)*(9.93-pH))*(1/335.26)*((NaCl+13.62)*(23-NaCl))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 498 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,NaCl)
argPar3 <- expand.grid(pH,NaCl)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],NaCl[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,NaCl,z2,col = 'green',xlab='T',ylab='NaCl',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,NaCl,z3,col = 'green',xlab='pH',ylab='NaCl',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Lactococcus piscium in/on Shrimp _cooked_
(gropin ID:498)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
