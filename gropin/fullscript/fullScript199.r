#############################
# start of Parameter script
#############################
T <- seq(4.004,36.963036963037,length.out=7)
pH <- seq(4.5045,7.99200799200799,length.out=7)
NaNO2 <- seq(0,149.85014985015,length.out=7)
aw <- seq(0.97097,0.998001998001998,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 199 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaNO2,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaNO2,aw) {
   mumax <-(-80.0191-(0.0709*T)+(255.1554*aw)-(7.3381*pH)+(0.0198*NaNO2)-(0.1321*T*aw)-(0.00315*T*pH)+(0.0000161*T*NaNO2)+(3.0076*aw*pH)+(0.00477*aw*NaNO2)-(0.00321*pH*NaNO2)+(0.00312*(T^2))-(155.3701*(aw^2))+(0.3088*(pH^2))-(0.0000005*(NaNO2^2)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaNO2'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 199 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:199)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,NaNO2))
argPar3 <- unique.data.frame(expand.grid(T,aw))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],NaNO2 = NaNO2[1],aw = aw[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],NaNO2 = argPar2[2],pH = pH[1],aw = aw[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],aw = argPar3[2],pH = pH[1],NaNO2 = NaNO2[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(NaNO2)>1 & length(aw)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaNO2,z2,col = 'green',xlab='T',ylab='NaNO2',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z3,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,NaNO2,aw))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(NaNO2)==1 & length(aw)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaNO2 =',round(NaNO2,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1) {
		persp(T,NaNO2,myZ,col = 'green',xlab='T',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(NaNO2)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaNO2)==1) {
		persp(pH,aw,myZ,col = 'green',xlab='pH',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1) {
		persp(NaNO2,aw,myZ,col = 'green',xlab='NaNO2',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
