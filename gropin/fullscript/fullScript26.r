#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=10)
aw <- seq(0.974974,0.991008991008991,length.out=10)
CO2_dissolved_ <- seq(0,2408.59140859141,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 26 
#############################
 
# constant coefficients for this model
Im <- 74.6
m1 <- -0.74
m2 <- 5.7000000000000002E-3
m3 <- -151
m4 <- 0.000124
m5 <- 4.62e-08
m6 <- 77
m7 <- -1e-05
m8 <- 0.76
m9 <- -0.0057
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(Im+(m1*T)+(m2*CO2_dissolved_)+(m3*aw)+(m4*(T^2))+(m5*(CO2_dissolved_^2))+(m6*(aw^2))+(m7*T*CO2_dissolved_)+(m8*T*aw)+(m9*CO2_dissolved_*aw))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 26 
#############################
titleText <-'Response surface _mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:26)'
argPar1 <- unique.data.frame(expand.grid(T,aw))
argPar2 <- unique.data.frame(expand.grid(T,CO2_dissolved_))
argPar3 <- unique.data.frame(expand.grid(aw,CO2_dissolved_))
z1 <- matrix(unlist(response_surface(T = argPar1[1],aw = argPar1[2],CO2_dissolved_ = CO2_dissolved_[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],CO2_dissolved_ = argPar2[2],aw = aw[1])),nrow=10)
z3 <- matrix(unlist(response_surface(aw = argPar3[1],CO2_dissolved_ = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(aw)>1 & length(CO2_dissolved_)>1) {
	par(mfrow = c(1,3))
	persp(T,aw,z1,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,CO2_dissolved_,z2,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(aw,CO2_dissolved_,z3,col = 'green',xlab='aw',ylab='CO2_dissolved_',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,aw,CO2_dissolved_))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(CO2_dissolved_)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',main=titleText,sub=paste('other variable: CO2_dissolved_ =',round(CO2_dissolved_,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1) {
		persp(T,CO2_dissolved_,myZ,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(aw,CO2_dissolved_,myZ,col = 'green',xlab='aw',ylab='CO2_dissolved_',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
