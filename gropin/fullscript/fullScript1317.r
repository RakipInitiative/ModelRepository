#############################
# start of Parameter script
#############################
T <- seq(15.015,51.948051948052,length.out=10)
NaCl <- seq(0.5005,3.996003996004,length.out=10)
pH <- seq(5.005,6.24375624375624,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1317 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,NaCl,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
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
titleText <-'Response surface _mu_max for
Clostridium perfringens in/on Meat _bulk_
(gropin ID:1317)'
argPar1 <- unique.data.frame(expand.grid(T,NaCl))
argPar2 <- unique.data.frame(expand.grid(T,pH))
argPar3 <- unique.data.frame(expand.grid(NaCl,pH))
z1 <- matrix(unlist(response_surface(T = argPar1[1],NaCl = argPar1[2],pH = pH[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],pH = argPar2[2],NaCl = NaCl[1])),nrow=10)
z3 <- matrix(unlist(response_surface(NaCl = argPar3[1],pH = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(NaCl)>1 & length(pH)>1) {
	par(mfrow = c(1,3))
	persp(T,NaCl,z1,col = 'green',xlab='T',ylab='NaCl',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,pH,z2,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(NaCl,pH,z3,col = 'green',xlab='NaCl',ylab='pH',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,NaCl,pH))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(pH)==1) {
		persp(T,NaCl,myZ,col = 'green',xlab='T',ylab='NaCl',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaCl)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(NaCl,pH,myZ,col = 'green',xlab='NaCl',ylab='pH',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
