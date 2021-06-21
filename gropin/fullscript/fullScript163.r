#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=7)
CO2_dissolved_ <- seq(0,1998.001998002,length.out=7)
aw <- seq(0.961238761238761,0.9892883,length.out=7)
NaL <- seq(0,2.997002997003,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 163 
#############################
 
# constant coefficients for this model
b <- 4.5231e-07
awmin <- 0.9485
Tmin <- -1.5389999999999999
CO2max <- 2476
NaLmax <- 3.7494
 
variables <- data.frame(T,CO2_dissolved_,aw,NaL)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CO2_dissolved_,aw,NaL) {
   mumax <-(1/(b*(aw-awmin)*(CO2max-CO2_dissolved_)*(T-Tmin)*(NaLmax-NaL)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CO2_dissolved_'],argumentsPar['aw'],argumentsPar['NaL']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 163 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Cooked meat model _in BHI_
(gropin ID:163)'
argPar1 <- unique.data.frame(expand.grid(T,CO2_dissolved_))
argPar2 <- unique.data.frame(expand.grid(T,aw))
argPar3 <- unique.data.frame(expand.grid(T,NaL))
z1 <- matrix(unlist(response_surface(T = argPar1[1],CO2_dissolved_ = argPar1[2],aw = aw[1],NaL = NaL[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],aw = argPar2[2],CO2_dissolved_ = CO2_dissolved_[1],NaL = NaL[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],NaL = argPar3[2],CO2_dissolved_ = CO2_dissolved_[1],aw = aw[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(CO2_dissolved_)>1 & length(aw)>1 & length(NaL)>1) {
	par(mfrow = c(1,3))
	persp(T,CO2_dissolved_,z1,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaL,z3,col = 'green',xlab='T',ylab='NaL',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,CO2_dissolved_,aw,NaL))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(aw)==1 & length(NaL)==1) {
		persp(T,CO2_dissolved_,myZ,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'NaL =',round(NaL,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(CO2_dissolved_)==1 & length(NaL)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: CO2_dissolved_ =',round(CO2_dissolved_,digits = 2), 'NaL =',round(NaL,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(CO2_dissolved_)==1 & length(aw)==1) {
		persp(T,NaL,myZ,col = 'green',xlab='T',ylab='NaL',zlab='ln_mu_max',main=titleText,sub=paste('other variable: CO2_dissolved_ =',round(CO2_dissolved_,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaL)==1) {
		persp(CO2_dissolved_,aw,myZ,col = 'green',xlab='CO2_dissolved_',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaL =',round(NaL,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1) {
		persp(CO2_dissolved_,NaL,myZ,col = 'green',xlab='CO2_dissolved_',ylab='NaL',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(CO2_dissolved_)==1) {
		persp(aw,NaL,myZ,col = 'green',xlab='aw',ylab='NaL',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'CO2_dissolved_ =',round(CO2_dissolved_,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
