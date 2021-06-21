#############################
# start of Parameter script
#############################
T <- seq(1.001,39.96003996004,length.out=7)
pH <- seq(4.004,6.99300699300699,length.out=7)
aw <- seq(0.9009,0.998001998001998,length.out=7)
La <- seq(0,4652.34765234765,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 504 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,La)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,La) {
   mumax <-(-18.851+0.2409*T+4.2628*pH+6.36771*sqrt(1-aw)-0.00232*(T^2)-0.31377*(pH^2)-43.1241*((sqrt(1-aw))^2)+La*(-3.5*(10^-5)-3*(10^-7)*T+0.000009*pH-0.00014*sqrt(1-aw)-1.2*(10^-9)*La))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['La']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 504 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Meat _ComBase data_
(gropin ID:504)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,aw))
argPar3 <- unique.data.frame(expand.grid(T,La))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],aw = aw[1],La = La[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],aw = argPar2[2],pH = pH[1],La = La[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],La = argPar3[2],pH = pH[1],aw = aw[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(aw)>1 & length(La)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,La,z3,col = 'green',xlab='T',ylab='La',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,aw,La))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(aw)==1 & length(La)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'La =',round(La,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(La)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'La =',round(La,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1) {
		persp(T,La,myZ,col = 'green',xlab='T',ylab='La',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(La)==1) {
		persp(pH,aw,myZ,col = 'green',xlab='pH',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'La =',round(La,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1) {
		persp(pH,La,myZ,col = 'green',xlab='pH',ylab='La',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1) {
		persp(aw,La,myZ,col = 'green',xlab='aw',ylab='La',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
