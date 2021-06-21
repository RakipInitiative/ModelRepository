#############################
# start of Parameter script
#############################
T <- seq(9.85985,29.8201798201798,length.out=10)
pH <- seq(5.63563,6.76323676323676,length.out=10)
NaCl <- seq(0.7007,4.55544455544456,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 335 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl) {
   mumax <-(-541.43+(3.2203*(10^5)/(273+T))-(4.9081*(10^7)/((273+T)^2))+0.1033*NaCl-0.0523*(NaCl^2)+3.9848*pH-0.3115*(pH^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 335 
#############################
titleText <-'Response surface ln_mu_max for
Salmonella spp. in/on Tryptic Soy Broth
(gropin ID:335)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,NaCl))
argPar3 <- unique.data.frame(expand.grid(pH,NaCl))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],NaCl = NaCl[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],NaCl = argPar2[2],pH = pH[1])),nrow=10)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],NaCl = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(NaCl)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaCl,z2,col = 'green',xlab='T',ylab='NaCl',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,NaCl,z3,col = 'green',xlab='pH',ylab='NaCl',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,NaCl))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(NaCl)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,NaCl,myZ,col = 'green',xlab='T',ylab='NaCl',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,NaCl,myZ,col = 'green',xlab='pH',ylab='NaCl',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
