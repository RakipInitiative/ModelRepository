############################# 
# start of Visualisation script Gropin ID 1052 
#############################
titleText <-'Response surface ln_mu_max for
Gluconobacter oxydans in/on Cold-filled ready to drink beverages
(gropin ID:1052)'
argPar1 <- unique.data.frame(expand.grid(pH,TA))
argPar2 <- unique.data.frame(expand.grid(pH,Sugar))
argPar3 <- unique.data.frame(expand.grid(pH,SB))
z1 <- matrix(unlist(response_surface(pH = argPar1[1],TA = argPar1[2],Sugar = Sugar[1],SB = SB[1],PS = PS[1])),nrow=5)
z2 <- matrix(unlist(response_surface(pH = argPar2[1],Sugar = argPar2[2],TA = TA[1],SB = SB[1],PS = PS[1])),nrow=5)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],SB = argPar3[2],TA = TA[1],Sugar = Sugar[1],PS = PS[1])),nrow=5)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(pH)>1 & length(TA)>1 & length(Sugar)>1 & length(SB)>1 & length(PS)>1) {
	par(mfrow = c(1,3))
	persp(pH,TA,z1,col = 'green',xlab='pH',ylab='TA',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,Sugar,z2,col = 'green',xlab='pH',ylab='Sugar',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,SB,z3,col = 'green',xlab='pH',ylab='SB',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(pH,TA,Sugar,SB,PS))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4],myPars[,5])),nrow=5)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(Sugar)==1 & length(SB)==1 & length(PS)==1) {
		persp(pH,TA,myZ,col = 'green',xlab='pH',ylab='TA',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Sugar =',round(Sugar,digits = 2), 'SB =',round(SB,digits = 2), 'PS =',round(PS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(TA)==1 & length(SB)==1 & length(PS)==1) {
		persp(pH,Sugar,myZ,col = 'green',xlab='pH',ylab='Sugar',zlab='ln_mu_max',main=titleText,sub=paste('other variable: TA =',round(TA,digits = 2), 'SB =',round(SB,digits = 2), 'PS =',round(PS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(TA)==1 & length(Sugar)==1 & length(PS)==1) {
		persp(pH,SB,myZ,col = 'green',xlab='pH',ylab='SB',zlab='ln_mu_max',main=titleText,sub=paste('other variable: TA =',round(TA,digits = 2), 'Sugar =',round(Sugar,digits = 2), 'PS =',round(PS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(TA)==1 & length(Sugar)==1 & length(SB)==1) {
		persp(pH,PS,myZ,col = 'green',xlab='pH',ylab='PS',zlab='ln_mu_max',main=titleText,sub=paste('other variable: TA =',round(TA,digits = 2), 'Sugar =',round(Sugar,digits = 2), 'SB =',round(SB,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(SB)==1 & length(PS)==1) {
		persp(TA,Sugar,myZ,col = 'green',xlab='TA',ylab='Sugar',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'SB =',round(SB,digits = 2), 'PS =',round(PS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(Sugar)==1 & length(PS)==1) {
		persp(TA,SB,myZ,col = 'green',xlab='TA',ylab='SB',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'Sugar =',round(Sugar,digits = 2), 'PS =',round(PS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(Sugar)==1 & length(SB)==1) {
		persp(TA,PS,myZ,col = 'green',xlab='TA',ylab='PS',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'Sugar =',round(Sugar,digits = 2), 'SB =',round(SB,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(TA)==1 & length(PS)==1) {
		persp(Sugar,SB,myZ,col = 'green',xlab='Sugar',ylab='SB',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'TA =',round(TA,digits = 2), 'PS =',round(PS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(TA)==1 & length(SB)==1) {
		persp(Sugar,PS,myZ,col = 'green',xlab='Sugar',ylab='PS',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'TA =',round(TA,digits = 2), 'SB =',round(SB,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(TA)==1 & length(Sugar)==1) {
		persp(SB,PS,myZ,col = 'green',xlab='SB',ylab='PS',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'TA =',round(TA,digits = 2), 'Sugar =',round(Sugar,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
