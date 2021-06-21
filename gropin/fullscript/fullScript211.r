#############################
# start of Parameter script
#############################
aw <- seq(0.928928,0.998001998001998,length.out=7)
pH <- seq(4.5045,7.49250749250749,length.out=7)
NaNO2 <- seq(0,149.85014985015,length.out=7)
T <- seq(4.004,36.963036963037,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 211 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(aw,pH,NaNO2,T)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(aw,pH,NaNO2,T) {
   mumax <-248.1902-342.9966*aw-18.9842*pH+0.0169*NaNO2-0.192*T*aw+0.0000215*T*NaNO2+12.5454*aw*pH-0.00231*pH*NaNO2+0.00203*(T^2)+121.0776*(aw^2)+0.4816*(pH^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['aw'],argumentsPar['pH'],argumentsPar['NaNO2'],argumentsPar['T']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 211 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:211)'
argPar1 <- unique.data.frame(expand.grid(aw,pH))
argPar2 <- unique.data.frame(expand.grid(aw,NaNO2))
argPar3 <- unique.data.frame(expand.grid(aw,T))
z1 <- matrix(unlist(response_surface(aw = argPar1[1],pH = argPar1[2],NaNO2 = NaNO2[1],T = T[1])),nrow=7)
z2 <- matrix(unlist(response_surface(aw = argPar2[1],NaNO2 = argPar2[2],pH = pH[1],T = T[1])),nrow=7)
z3 <- matrix(unlist(response_surface(aw = argPar3[1],T = argPar3[2],pH = pH[1],NaNO2 = NaNO2[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(aw)>1 & length(pH)>1 & length(NaNO2)>1 & length(T)>1) {
	par(mfrow = c(1,3))
	persp(aw,pH,z1,col = 'green',xlab='aw',ylab='pH',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(aw,NaNO2,z2,col = 'green',xlab='aw',ylab='NaNO2',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(aw,T,z3,col = 'green',xlab='aw',ylab='T',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(aw,pH,NaNO2,T))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(NaNO2)==1 & length(T)==1) {
		persp(aw,pH,myZ,col = 'green',xlab='aw',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaNO2 =',round(NaNO2,digits = 2), 'T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(T)==1) {
		persp(aw,NaNO2,myZ,col = 'green',xlab='aw',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(NaNO2)==1) {
		persp(aw,T,myZ,col = 'green',xlab='aw',ylab='T',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1 & length(T)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1 & length(NaNO2)==1) {
		persp(pH,T,myZ,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1 & length(pH)==1) {
		persp(NaNO2,T,myZ,col = 'green',xlab='NaNO2',ylab='T',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
