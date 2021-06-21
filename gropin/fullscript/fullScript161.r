#############################
# start of Parameter script
#############################
T <- seq(10.01,24.975024975025,length.out=10)
CitrA <- seq(0,0.3996003996004,length.out=10)
AscorbA <- seq(0,0.3996003996004,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 161 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CitrA,AscorbA)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,CitrA,AscorbA) {
   mumax <-(0.1873+(0.7077*T)-(0.4681*CitrA)-(0.0706*AscorbA)+(0.03353*(T^2))+(0.5058*(CitrA^2))-(1.1107*T*CitrA)-(0.4981*T*AscorbA)+(0.3896*CitrA*AscorbA))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CitrA'],argumentsPar['AscorbA']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 161 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Tryptic Soy Broth
(gropin ID:161)'
argPar1 <- unique.data.frame(expand.grid(T,CitrA))
argPar2 <- unique.data.frame(expand.grid(T,AscorbA))
argPar3 <- unique.data.frame(expand.grid(CitrA,AscorbA))
z1 <- matrix(unlist(response_surface(T = argPar1[1],CitrA = argPar1[2],AscorbA = AscorbA[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],AscorbA = argPar2[2],CitrA = CitrA[1])),nrow=10)
z3 <- matrix(unlist(response_surface(CitrA = argPar3[1],AscorbA = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(CitrA)>1 & length(AscorbA)>1) {
	par(mfrow = c(1,3))
	persp(T,CitrA,z1,col = 'green',xlab='T',ylab='CitrA',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,AscorbA,z2,col = 'green',xlab='T',ylab='AscorbA',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(CitrA,AscorbA,z3,col = 'green',xlab='CitrA',ylab='AscorbA',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,CitrA,AscorbA))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(AscorbA)==1) {
		persp(T,CitrA,myZ,col = 'green',xlab='T',ylab='CitrA',zlab='_mu_max',main=titleText,sub=paste('other variable: AscorbA =',round(AscorbA,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(CitrA)==1) {
		persp(T,AscorbA,myZ,col = 'green',xlab='T',ylab='AscorbA',zlab='_mu_max',main=titleText,sub=paste('other variable: CitrA =',round(CitrA,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(CitrA,AscorbA,myZ,col = 'green',xlab='CitrA',ylab='AscorbA',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
