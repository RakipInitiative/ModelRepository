#############################
# start of Parameter script
#############################
T <- seq(4.004,36.963036963037,length.out=7)
aw <- seq(0.928928,0.998001998001998,length.out=7)
pH <- seq(4.5045,7.49250749250749,length.out=7)
NaNO2 <- seq(0,149.85014985015,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 203 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,aw,pH,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,pH,NaNO2) {
   mumax <-252.833+0.1418*T-358.214*aw-18.4395*pH+0.0151*NaNO2-0.3653*T*aw+0.00452*T*pH+0.0000169*T*NaNO2+11.8359*aw*pH+0.00437*aw*NaNO2-0.00269*pH*NaNO2+0.00201*(T^2)+132.4864*(aw^2)+0.4881*(pH^2)+0.0000005*(NaNO2^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['pH'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 203 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:203)'
argPar1 <- unique.data.frame(expand.grid(T,aw))
argPar2 <- unique.data.frame(expand.grid(T,pH))
argPar3 <- unique.data.frame(expand.grid(T,NaNO2))
z1 <- matrix(unlist(response_surface(T = argPar1[1],aw = argPar1[2],pH = pH[1],NaNO2 = NaNO2[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],pH = argPar2[2],aw = aw[1],NaNO2 = NaNO2[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],NaNO2 = argPar3[2],aw = aw[1],pH = pH[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(aw)>1 & length(pH)>1 & length(NaNO2)>1) {
	par(mfrow = c(1,3))
	persp(T,aw,z1,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,pH,z2,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaNO2,z3,col = 'green',xlab='T',ylab='NaNO2',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,aw,pH,NaNO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(pH)==1 & length(NaNO2)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1 & length(NaNO2)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1 & length(pH)==1) {
		persp(T,NaNO2,myZ,col = 'green',xlab='T',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaNO2)==1) {
		persp(aw,pH,myZ,col = 'green',xlab='aw',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1) {
		persp(aw,NaNO2,myZ,col = 'green',xlab='aw',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
