#############################
# start of Parameter script
#############################
pH <- seq(4.5045,7.99200799200799,length.out=7)
NaNO2 <- seq(0,149.85014985015,length.out=7)
T <- seq(4.004,36.963036963037,length.out=7)
aw <- seq(0.97097,0.998001998001998,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 207 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,NaNO2,T,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,NaNO2,T,aw) {
   mumax <-34.3435-4.4105*pH+0.0277*NaNO2-0.2272*T*aw-0.00366*pH*NaNO2+0.00319*(T^2)-13.9892*(aw^2)+0.3089*(pH^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['NaNO2'],argumentsPar['T'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
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
