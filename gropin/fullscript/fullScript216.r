#############################
# start of Parameter script
#############################
T <- seq(4.004,24.975024975025,length.out=10)
Phe <- seq(0,33.966033966034,length.out=10)
NaCl <- seq(0,7.99200799200799,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 216 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,Phe,NaCl)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,Phe,NaCl) {
   mumax <-(61.354-11.657*T+10.159*Phe+14.752*NaCl-0.147*(T*Phe)-0.863*(T*NaCl)+0.423*(Phe*NaCl)+0.321*(T^2)-0.231*(Phe^2)+0.485*(NaCl^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['Phe'],argumentsPar['NaCl']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 216 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Salmon fillets
(gropin ID:216)'
argPar1 <- unique.data.frame(expand.grid(T,Phe))
argPar2 <- unique.data.frame(expand.grid(T,NaCl))
argPar3 <- unique.data.frame(expand.grid(Phe,NaCl))
z1 <- matrix(unlist(response_surface(T = argPar1[1],Phe = argPar1[2],NaCl = NaCl[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],NaCl = argPar2[2],Phe = Phe[1])),nrow=10)
z3 <- matrix(unlist(response_surface(Phe = argPar3[1],NaCl = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(Phe)>1 & length(NaCl)>1) {
	par(mfrow = c(1,3))
	persp(T,Phe,z1,col = 'green',xlab='T',ylab='Phe',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaCl,z2,col = 'green',xlab='T',ylab='NaCl',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(Phe,NaCl,z3,col = 'green',xlab='Phe',ylab='NaCl',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,Phe,NaCl))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(NaCl)==1) {
		persp(T,Phe,myZ,col = 'green',xlab='T',ylab='Phe',zlab='_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(Phe)==1) {
		persp(T,NaCl,myZ,col = 'green',xlab='T',ylab='NaCl',zlab='_mu_max',main=titleText,sub=paste('other variable: Phe =',round(Phe,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(Phe,NaCl,myZ,col = 'green',xlab='Phe',ylab='NaCl',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
