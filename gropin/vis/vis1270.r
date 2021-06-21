############################# 
# start of Visualisation script Gropin ID 1270 
#############################
titleText <-'Response surface ln_mu_max for
Yersinia enterocolitica in/on Sausages  Italian-fresh pork _80%_
(gropin ID:1270)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,Hours))
argPar3 <- unique.data.frame(expand.grid(pH,Hours))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],Hours = Hours[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],Hours = argPar2[2],pH = pH[1])),nrow=10)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],Hours = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(Hours)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,Hours,z2,col = 'green',xlab='T',ylab='Hours',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,Hours,z3,col = 'green',xlab='pH',ylab='Hours',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,Hours))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(Hours)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Hours =',round(Hours,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,Hours,myZ,col = 'green',xlab='T',ylab='Hours',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,Hours,myZ,col = 'green',xlab='pH',ylab='Hours',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
