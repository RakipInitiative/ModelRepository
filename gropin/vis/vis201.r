############################# 
# start of Visualisation script Gropin ID 201 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:201)'
argPar1 <- unique.data.frame(expand.grid(T,NaCl))
argPar2 <- unique.data.frame(expand.grid(T,pH))
argPar3 <- unique.data.frame(expand.grid(T,NaNO2))
z1 <- matrix(unlist(response_surface(T = argPar1[1],NaCl = argPar1[2],pH = pH[1],NaNO2 = NaNO2[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],pH = argPar2[2],NaCl = NaCl[1],NaNO2 = NaNO2[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],NaNO2 = argPar3[2],NaCl = NaCl[1],pH = pH[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(NaCl)>1 & length(pH)>1 & length(NaNO2)>1) {
	par(mfrow = c(1,3))
	persp(T,NaCl,z1,col = 'green',xlab='T',ylab='NaCl',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,pH,z2,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaNO2,z3,col = 'green',xlab='T',ylab='NaNO2',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,NaCl,pH,NaNO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(pH)==1 & length(NaNO2)==1) {
		persp(T,NaCl,myZ,col = 'green',xlab='T',ylab='NaCl',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaCl)==1 & length(NaNO2)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaCl)==1 & length(pH)==1) {
		persp(T,NaNO2,myZ,col = 'green',xlab='T',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaNO2)==1) {
		persp(NaCl,pH,myZ,col = 'green',xlab='NaCl',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1) {
		persp(NaCl,NaNO2,myZ,col = 'green',xlab='NaCl',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaCl)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
