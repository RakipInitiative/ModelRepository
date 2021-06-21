#############################
# start of Parameter script
#############################
Fructose <- seq(0.1001,49.95004995005,length.out=10)
Ethanol <- seq(0,11.988011988012,length.out=10)
pH <- seq(2.5025,7.99200799200799,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1033 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(Fructose,Ethanol,pH)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(Fructose,Ethanol,pH) {
   mumax <-log(0.45)-0.0146*((pH-7)^2)-40.85*((sqrt(1-( 55.556/(55.556+(Ethanol*0.7893/46)+(Fructose*10/180.16))))-0.05)^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['Fructose'],argumentsPar['Ethanol'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1033 
#############################
titleText <-'Response surface ln_mu_max for
Saccharomyces cerevisiae in/on Wines, beverages, fruit concentrates, preserves
(gropin ID:1033)'
argPar1 <- unique.data.frame(expand.grid(Fructose,Ethanol))
argPar2 <- unique.data.frame(expand.grid(Fructose,pH))
argPar3 <- unique.data.frame(expand.grid(Ethanol,pH))
z1 <- matrix(unlist(response_surface(Fructose = argPar1[1],Ethanol = argPar1[2],pH = pH[1])),nrow=10)
z2 <- matrix(unlist(response_surface(Fructose = argPar2[1],pH = argPar2[2],Ethanol = Ethanol[1])),nrow=10)
z3 <- matrix(unlist(response_surface(Ethanol = argPar3[1],pH = argPar3[2],Fructose = Fructose[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(Fructose)>1 & length(Ethanol)>1 & length(pH)>1) {
	par(mfrow = c(1,3))
	persp(Fructose,Ethanol,z1,col = 'green',xlab='Fructose',ylab='Ethanol',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(Fructose,pH,z2,col = 'green',xlab='Fructose',ylab='pH',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(Ethanol,pH,z3,col = 'green',xlab='Ethanol',ylab='pH',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(Fructose,Ethanol,pH))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(pH)==1) {
		persp(Fructose,Ethanol,myZ,col = 'green',xlab='Fructose',ylab='Ethanol',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Ethanol)==1) {
		persp(Fructose,pH,myZ,col = 'green',xlab='Fructose',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Ethanol =',round(Ethanol,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Fructose)==1) {
		persp(Ethanol,pH,myZ,col = 'green',xlab='Ethanol',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Fructose =',round(Fructose,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
