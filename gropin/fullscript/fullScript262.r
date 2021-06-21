#############################
# start of Parameter script
#############################
T <- seq(4.004,24.975024975025,length.out=10)
pH <- seq(3.6036,4.4955044955045,length.out=10)
Ac <- seq(0,3.996003996004,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 262 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Ac)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,Ac) {
   mumax <-(2.035+(0.818*T)+((-6.917)*(Ac/(1+10^(pH-4.76))))+(0.0009*(T^2))+(0.358*((Ac/(1+10^(pH-4.76)))^2))+((-0.196)*T*pH)+(1.259*pH*(Ac/(1+10^(pH-4.76)))))^2/24

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Ac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 262 
#############################
titleText <-'Response surface _mu_max for
Lactic acid bacteria in/on Deli salads _Fava-, Pepper-, Cheese salad_
(gropin ID:262)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,Ac))
argPar3 <- unique.data.frame(expand.grid(pH,Ac))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],Ac = Ac[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],Ac = argPar2[2],pH = pH[1])),nrow=10)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],Ac = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(Ac)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,Ac,z2,col = 'green',xlab='T',ylab='Ac',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,Ac,z3,col = 'green',xlab='pH',ylab='Ac',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,Ac))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(Ac)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',main=titleText,sub=paste('other variable: Ac =',round(Ac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,Ac,myZ,col = 'green',xlab='T',ylab='Ac',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,Ac,myZ,col = 'green',xlab='pH',ylab='Ac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
