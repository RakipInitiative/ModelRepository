#############################
# start of Parameter script
#############################
T <- seq(20.02,39.96003996004,length.out=10)
pH <- seq(3.5035,4.995004995005,length.out=10)
aw <- seq(0.919919,0.969030969030969,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 243 
#############################
 
# constant coefficients for this model
moptdays1 <- 21.89
awmin <- 0.906
pHmin <- -101.1
pHmax <- 5
pHopt <- 4.81
TmaxoC <- 46.19
TminoC <- 14.61
ToptoC <- 34.36
 
variables <- data.frame(T,pH,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-moptdays1*((aw-awmin)/(1-awmin))*((((T-TminoC)^2)*(T-TmaxoC))/((ToptoC-TminoC)*((ToptoC-TminoC)*(T-ToptoC)-(ToptoC-TmaxoC)*(ToptoC+TminoC-2*T))))*((((pH-pHmin)^2)*(pH-pHmax))/((pHopt-pHmin)*((pHopt-pHmin)*(pH-pHopt)-(pHopt-pHmax)*(pHopt+pHmin-2*pH))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 243 
#############################
titleText <-'Response surface _mu_max for
Monascus ruber in/on Malt extract agar
(gropin ID:243)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,aw))
argPar3 <- unique.data.frame(expand.grid(pH,aw))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],aw = aw[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],aw = argPar2[2],pH = pH[1])),nrow=10)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],aw = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(aw)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,aw))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(aw)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,aw,myZ,col = 'green',xlab='pH',ylab='aw',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
