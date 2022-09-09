#############################
# start of Parameter script
#############################
T <- seq(0,9.99000999000999,length.out=7)
pH <- seq(5.5055,7.09290709290709,length.out=7)
CO2 <- seq(0,79.9200799200799,length.out=7)
O2 <- seq(0,99.9000999000999,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 448 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,CO2,O2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,CO2,O2) {
   mumax <-(-4.50+(-0.829)*T+(-0.0151)*(T^2)+(-0.00122)*T*CO2+0.184*T*pH+(-0.00114)*pH*O2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['CO2'],argumentsPar['O2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 448 
#############################
titleText <-'Response surface ln_mu_max for
Aeromonas hydrophila in/on Modified meat
(gropin ID:448)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,CO2))
argPar3 <- unique.data.frame(expand.grid(T,O2))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],CO2 = CO2[1],O2 = O2[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],CO2 = argPar2[2],pH = pH[1],O2 = O2[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],O2 = argPar3[2],pH = pH[1],CO2 = CO2[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(CO2)>1 & length(O2)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,CO2,z2,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,O2,z3,col = 'green',xlab='T',ylab='O2',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,CO2,O2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(CO2)==1 & length(O2)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: CO2 =',round(CO2,digits = 2), 'O2 =',round(O2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(O2)==1) {
		persp(T,CO2,myZ,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'O2 =',round(O2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(CO2)==1) {
		persp(T,O2,myZ,col = 'green',xlab='T',ylab='O2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(O2)==1) {
		persp(pH,CO2,myZ,col = 'green',xlab='pH',ylab='CO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'O2 =',round(O2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(CO2)==1) {
		persp(pH,O2,myZ,col = 'green',xlab='pH',ylab='O2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1) {
		persp(CO2,O2,myZ,col = 'green',xlab='CO2',ylab='O2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################