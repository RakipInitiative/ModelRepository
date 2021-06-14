#############################
# start of Parameter script
#############################
T <- seq(20.02,39.96003996004,length.out=21)
pH <- seq(3.5035,4.995004995005,length.out=21)
aw <- seq(0.919919,0.969030969030969,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 243 
#############################
 
# constant coefficients for this model
moptdays1 <- 21.89
awmin <- 0.906
pHmin <- -101.1
pHmax <- 5
pHopt <- 4.81
TmaxoC <- 46.19
TminoC <- 14.61
ToptoC <- 34.36
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-moptdays1*((aw-awmin)/(1-awmin))*((((T-TminoC)^2)*(T-TmaxoC))/((ToptoC-TminoC)*((ToptoC-TminoC)*(T-ToptoC)-(ToptoC-TmaxoC)*(ToptoC+TminoC-2*T))))*((((pH-pHmin)^2)*(pH-pHmax))/((pHopt-pHmin)*((pHopt-pHmin)*(pH-pHopt)-(pHopt-pHmax)*(pHopt+pHmin-2*pH))))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 243 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,aw)
argPar3 <- expand.grid(pH,aw)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],aw[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Monascus ruber in/on Malt extract agar
(gropin ID:243)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
