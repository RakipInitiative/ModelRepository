#############################
# start of Parameter script
#############################
S_S <- seq(0.83083,2.4975024975025,length.out=10)
Ac <- seq(1.8018,2.7972027972028,length.out=10)
pH <- seq(3.5035,3.996003996004,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 488 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(S_S,Ac,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(S_S,Ac,pH) {
   mumax <-(4.53+3.29*((S_S-1.66)/0.6)+2.94*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)+0.36*(((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)^2)+0.82*((S_S-1.66)/0.6)*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['S_S'],argumentsPar['Ac'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 488 
#############################
titleText <-'Response surface ln_mu_max for
Zygosaccharomyces bailii in/on Yeast Nitrogen Broth
(gropin ID:488)'
argPar1 <- unique.data.frame(expand.grid(S_S,Ac))
argPar2 <- unique.data.frame(expand.grid(S_S,pH))
argPar3 <- unique.data.frame(expand.grid(Ac,pH))
z1 <- matrix(unlist(response_surface(S_S = argPar1[1],Ac = argPar1[2],pH = pH[1])),nrow=10)
z2 <- matrix(unlist(response_surface(S_S = argPar2[1],pH = argPar2[2],Ac = Ac[1])),nrow=10)
z3 <- matrix(unlist(response_surface(Ac = argPar3[1],pH = argPar3[2],S_S = S_S[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(S_S)>1 & length(Ac)>1 & length(pH)>1) {
	par(mfrow = c(1,3))
	persp(S_S,Ac,z1,col = 'green',xlab='S_S',ylab='Ac',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(S_S,pH,z2,col = 'green',xlab='S_S',ylab='pH',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(Ac,pH,z3,col = 'green',xlab='Ac',ylab='pH',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(S_S,Ac,pH))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(pH)==1) {
		persp(S_S,Ac,myZ,col = 'green',xlab='S_S',ylab='Ac',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Ac)==1) {
		persp(S_S,pH,myZ,col = 'green',xlab='S_S',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Ac =',round(Ac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(S_S)==1) {
		persp(Ac,pH,myZ,col = 'green',xlab='Ac',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: S_S =',round(S_S,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################