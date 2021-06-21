############################# 
# start of Visualisation script Gropin ID 207 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:207)'
argPar1 <- unique.data.frame(expand.grid(pH,NaNO2))
argPar2 <- unique.data.frame(expand.grid(pH,T))
argPar3 <- unique.data.frame(expand.grid(pH,aw))
z1 <- matrix(unlist(response_surface(pH = argPar1[1],NaNO2 = argPar1[2],T = T[1],aw = aw[1])),nrow=7)
z2 <- matrix(unlist(response_surface(pH = argPar2[1],T = argPar2[2],NaNO2 = NaNO2[1],aw = aw[1])),nrow=7)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],aw = argPar3[2],NaNO2 = NaNO2[1],T = T[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(pH)>1 & length(NaNO2)>1 & length(T)>1 & length(aw)>1) {
	par(mfrow = c(1,3))
	persp(pH,NaNO2,z1,col = 'green',xlab='pH',ylab='NaNO2',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,T,z2,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(pH,NaNO2,T,aw))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(T)==1 & length(aw)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaNO2)==1 & length(aw)==1) {
		persp(pH,T,myZ,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaNO2 =',round(NaNO2,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaNO2)==1 & length(T)==1) {
		persp(pH,aw,myZ,col = 'green',xlab='pH',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaNO2 =',round(NaNO2,digits = 2), 'T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1) {
		persp(NaNO2,T,myZ,col = 'green',xlab='NaNO2',ylab='T',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(T)==1) {
		persp(NaNO2,aw,myZ,col = 'green',xlab='NaNO2',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(NaNO2)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
