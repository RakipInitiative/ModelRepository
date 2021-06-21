############################# 
# start of Visualisation script Gropin ID 157 
#############################
titleText <-'Response surface Sqr_mu_max for
Lactobacillus sake in/on Cooked meat model _in modified BHI_
(gropin ID:157)'
argPar1 <- unique.data.frame(expand.grid(T,aw))
argPar2 <- unique.data.frame(expand.grid(T,NaL))
argPar3 <- unique.data.frame(expand.grid(T,CO2_dissolved_))
z1 <- matrix(unlist(response_surface(T = argPar1[1],aw = argPar1[2],NaL = NaL[1],CO2_dissolved_ = CO2_dissolved_[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],NaL = argPar2[2],aw = aw[1],CO2_dissolved_ = CO2_dissolved_[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],CO2_dissolved_ = argPar3[2],aw = aw[1],NaL = NaL[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(aw)>1 & length(NaL)>1 & length(CO2_dissolved_)>1) {
	par(mfrow = c(1,3))
	persp(T,aw,z1,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaL,z2,col = 'green',xlab='T',ylab='NaL',zlab='Sqr_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,CO2_dissolved_,z3,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='Sqr_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,aw,NaL,CO2_dissolved_))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(NaL)==1 & length(CO2_dissolved_)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: NaL =',round(NaL,digits = 2), 'CO2_dissolved_ =',round(CO2_dissolved_,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1 & length(CO2_dissolved_)==1) {
		persp(T,NaL,myZ,col = 'green',xlab='T',ylab='NaL',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'CO2_dissolved_ =',round(CO2_dissolved_,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1 & length(NaL)==1) {
		persp(T,CO2_dissolved_,myZ,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'NaL =',round(NaL,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(CO2_dissolved_)==1) {
		persp(aw,NaL,myZ,col = 'green',xlab='aw',ylab='NaL',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'CO2_dissolved_ =',round(CO2_dissolved_,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaL)==1) {
		persp(aw,CO2_dissolved_,myZ,col = 'green',xlab='aw',ylab='CO2_dissolved_',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaL =',round(NaL,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1) {
		persp(NaL,CO2_dissolved_,myZ,col = 'green',xlab='NaL',ylab='CO2_dissolved_',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
