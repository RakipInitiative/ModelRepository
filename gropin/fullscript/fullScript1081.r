#############################
# start of Parameter script
#############################
T <- seq(45.045,94.9050949050949,length.out=7)
Time <- seq(0,59.9400599400599,length.out=7)
TA <- seq(0,7.99200799200799,length.out=7)
SS <- seq(0,79.9200799200799,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1081 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Time,TA,SS)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Time,TA,SS) {
   mumax <-(-14.54+(0.64*T)+0.40*Time-0.21*TA-0.02*SS)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Time'],argumentsPar['TA'],argumentsPar['SS']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1081 
#############################
titleText <-'Response surface _mu_max for
Ascorbic acid degradation in/on Fruit juices
(gropin ID:1081)'
argPar1 <- unique.data.frame(expand.grid(T,Time))
argPar2 <- unique.data.frame(expand.grid(T,TA))
argPar3 <- unique.data.frame(expand.grid(T,SS))
z1 <- matrix(unlist(response_surface(T = argPar1[1],Time = argPar1[2],TA = TA[1],SS = SS[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],TA = argPar2[2],Time = Time[1],SS = SS[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],SS = argPar3[2],Time = Time[1],TA = TA[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(Time)>1 & length(TA)>1 & length(SS)>1) {
	par(mfrow = c(1,3))
	persp(T,Time,z1,col = 'green',xlab='T',ylab='Time',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,TA,z2,col = 'green',xlab='T',ylab='TA',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,SS,z3,col = 'green',xlab='T',ylab='SS',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,Time,TA,SS))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(TA)==1 & length(SS)==1) {
		persp(T,Time,myZ,col = 'green',xlab='T',ylab='Time',zlab='_mu_max',main=titleText,sub=paste('other variable: TA =',round(TA,digits = 2), 'SS =',round(SS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Time)==1 & length(SS)==1) {
		persp(T,TA,myZ,col = 'green',xlab='T',ylab='TA',zlab='_mu_max',main=titleText,sub=paste('other variable: Time =',round(Time,digits = 2), 'SS =',round(SS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Time)==1 & length(TA)==1) {
		persp(T,SS,myZ,col = 'green',xlab='T',ylab='SS',zlab='_mu_max',main=titleText,sub=paste('other variable: Time =',round(Time,digits = 2), 'TA =',round(TA,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(SS)==1) {
		persp(Time,TA,myZ,col = 'green',xlab='Time',ylab='TA',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'SS =',round(SS,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(TA)==1) {
		persp(Time,SS,myZ,col = 'green',xlab='Time',ylab='SS',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'TA =',round(TA,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(Time)==1) {
		persp(TA,SS,myZ,col = 'green',xlab='TA',ylab='SS',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'Time =',round(Time,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
