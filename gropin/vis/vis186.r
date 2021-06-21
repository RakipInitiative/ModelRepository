############################# 
# start of Visualisation script Gropin ID 186 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:186)'
argPar1 <- unique.data.frame(expand.grid(pH,T))
argPar2 <- unique.data.frame(expand.grid(pH,CO2))
argPar3 <- unique.data.frame(expand.grid(T,CO2))
z1 <- matrix(unlist(response_surface(pH = argPar1[1],T = argPar1[2],CO2 = CO2[1])),nrow=10)
z2 <- matrix(unlist(response_surface(pH = argPar2[1],CO2 = argPar2[2],T = T[1])),nrow=10)
z3 <- matrix(unlist(response_surface(T = argPar3[1],CO2 = argPar3[2],pH = pH[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(pH)>1 & length(T)>1 & length(CO2)>1) {
	par(mfrow = c(1,3))
	persp(pH,T,z1,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,CO2,z2,col = 'green',xlab='pH',ylab='CO2',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,CO2,z3,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(pH,T,CO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(CO2)==1) {
		persp(pH,T,myZ,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',main=titleText,sub=paste('other variable: CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,CO2,myZ,col = 'green',xlab='pH',ylab='CO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,CO2,myZ,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
