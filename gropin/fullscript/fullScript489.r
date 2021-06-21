#############################
# start of Parameter script
#############################
Fruct <- seq(7.007,31.968031968032,length.out=7)
NaCl <- seq(2.6026,4.1958041958042,length.out=7)
pH <- seq(3.5035,3.996003996004,length.out=7)
Ac <- seq(1.8018,2.7972027972028,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 489 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Fruct,NaCl,pH,Ac)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(Fruct,NaCl,pH,Ac) {
   mumax <-4.76+3.79*((Fruct-19.5)/17.7)+1.47*((NaCl-3.4)/1.1)-1.62*((pH-3.8)/0.4)+3.21*((Ac-2.3)/0.7)+0.59*((Fruct-19.5)/17.7)*((NaCl-3.4)/1.1)-0.44*((Fruct-19.5)/17.7)*((pH-3.8)/0.4)+0.82*((Fruct-19.5)/17.7)*((Ac-2.3)/0.7)-0.24*((NaCl-3.4)/1.1)*((pH-3.8)/0.4)+0.46*((NaCl-3.4)/1.1)*((Ac-2.3)/0.7)-0.74*((pH-3.8)/0.4)*((Ac-2.3)/0.7)+0.39*(((Ac-2.3)/0.7)^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Fruct'],argumentsPar['NaCl'],argumentsPar['pH'],argumentsPar['Ac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 489 
#############################
titleText <-'Response surface ln_mu_max for
Zygosaccharomyces bailii in/on Yeast Nitrogen Broth
(gropin ID:489)'
argPar1 <- unique.data.frame(expand.grid(Fruct,NaCl))
argPar2 <- unique.data.frame(expand.grid(Fruct,pH))
argPar3 <- unique.data.frame(expand.grid(Fruct,Ac))
z1 <- matrix(unlist(response_surface(Fruct = argPar1[1],NaCl = argPar1[2],pH = pH[1],Ac = Ac[1])),nrow=7)
z2 <- matrix(unlist(response_surface(Fruct = argPar2[1],pH = argPar2[2],NaCl = NaCl[1],Ac = Ac[1])),nrow=7)
z3 <- matrix(unlist(response_surface(Fruct = argPar3[1],Ac = argPar3[2],NaCl = NaCl[1],pH = pH[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(Fruct)>1 & length(NaCl)>1 & length(pH)>1 & length(Ac)>1) {
	par(mfrow = c(1,3))
	persp(Fruct,NaCl,z1,col = 'green',xlab='Fruct',ylab='NaCl',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(Fruct,pH,z2,col = 'green',xlab='Fruct',ylab='pH',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(Fruct,Ac,z3,col = 'green',xlab='Fruct',ylab='Ac',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(Fruct,NaCl,pH,Ac))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(pH)==1 & length(Ac)==1) {
		persp(Fruct,NaCl,myZ,col = 'green',xlab='Fruct',ylab='NaCl',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'Ac =',round(Ac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaCl)==1 & length(Ac)==1) {
		persp(Fruct,pH,myZ,col = 'green',xlab='Fruct',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2), 'Ac =',round(Ac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaCl)==1 & length(pH)==1) {
		persp(Fruct,Ac,myZ,col = 'green',xlab='Fruct',ylab='Ac',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Fruct)==1 & length(Ac)==1) {
		persp(NaCl,pH,myZ,col = 'green',xlab='NaCl',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Fruct =',round(Fruct,digits = 2), 'Ac =',round(Ac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Fruct)==1 & length(pH)==1) {
		persp(NaCl,Ac,myZ,col = 'green',xlab='NaCl',ylab='Ac',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Fruct =',round(Fruct,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Fruct)==1 & length(NaCl)==1) {
		persp(pH,Ac,myZ,col = 'green',xlab='pH',ylab='Ac',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Fruct =',round(Fruct,digits = 2), 'NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
