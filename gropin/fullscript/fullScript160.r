#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=7)
NaL <- seq(0,2.997002997003,length.out=7)
aw <- seq(0.961238761238761,0.9892883,length.out=7)
CO2 <- seq(0,1984.01598401598,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 160 
#############################
 
# constant coefficients for this model
ll <- 304.49
l1 <- -24.22
l2 <- -299.62
l3 <- 27.63
l4 <- 0.00136
l5 <- 0.0377
l7 <- 0.2
l9 <- 23.68
l10 <- -0.18
l11 <- -0.00017
l12 <- -27.14
l14 <- 0.00073
 
variables <- data.frame(T,NaL,aw,CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,NaL,aw,CO2) {
   mumax <-(ll+(l1*T)+(l2*aw)+(l3*NaL)+(l4*CO2)+(l5*(T^2))+(l7*(NaL^2))+(l9*T*aw)+(l10*T*NaL)+(l11*T*CO2)+(l12*aw*NaL)+(l14*NaL*CO2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['NaL'],argumentsPar['aw'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 160 
#############################
titleText <-'Response surface Sqr_mu_max for
Lactobacillus sake in/on Cooked meat model _in modified BHI_
(gropin ID:160)'
argPar1 <- unique.data.frame(expand.grid(T,NaL))
argPar2 <- unique.data.frame(expand.grid(T,aw))
argPar3 <- unique.data.frame(expand.grid(T,CO2))
z1 <- matrix(unlist(response_surface(T = argPar1[1],NaL = argPar1[2],aw = aw[1],CO2 = CO2[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],aw = argPar2[2],NaL = NaL[1],CO2 = CO2[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],CO2 = argPar3[2],NaL = NaL[1],aw = aw[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(NaL)>1 & length(aw)>1 & length(CO2)>1) {
	par(mfrow = c(1,3))
	persp(T,NaL,z1,col = 'green',xlab='T',ylab='NaL',zlab='Sqr_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,CO2,z3,col = 'green',xlab='T',ylab='CO2',zlab='Sqr_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,NaL,aw,CO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(aw)==1 & length(CO2)==1) {
		persp(T,NaL,myZ,col = 'green',xlab='T',ylab='NaL',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaL)==1 & length(CO2)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: NaL =',round(NaL,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaL)==1 & length(aw)==1) {
		persp(T,CO2,myZ,col = 'green',xlab='T',ylab='CO2',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: NaL =',round(NaL,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(CO2)==1) {
		persp(NaL,aw,myZ,col = 'green',xlab='NaL',ylab='aw',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1) {
		persp(NaL,CO2,myZ,col = 'green',xlab='NaL',ylab='CO2',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaL)==1) {
		persp(aw,CO2,myZ,col = 'green',xlab='aw',ylab='CO2',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaL =',round(NaL,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
