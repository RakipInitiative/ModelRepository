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
# start of Model script Gropin ID 30 
#############################
 
# constant coefficients for this model
Il <- -18000
I1 <- -35.6
I2 <- 0.47
I3 <- 37000
I4 <- 0.053
I5 <- 7.6e-08
I6 <- -19000
I7 <- -0.00035
I8 <- 34
I9 <- -0.46
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(Il+(I1*T)+(I2*CO2_dissolved_)+(I3*aw)+(I4*(T^2))+(I5*(CO2_dissolved_^2))+(I6*(aw^2))+(I7*T*CO2_dissolved_)+(I8*T*aw)+(I9*CO2_dissolved_*aw))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 30 
#############################
titleText <-'Response surface Sqr_mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:30)'
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
	persp(T,aw,z1,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,CO2_dissolved_,z2,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='Sqr_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(aw,CO2_dissolved_,z3,col = 'green',xlab='aw',ylab='CO2_dissolved_',zlab='Sqr_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,aw,CO2_dissolved_))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(CO2_dissolved_)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: CO2_dissolved_ =',round(CO2_dissolved_,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1) {
		persp(T,CO2_dissolved_,myZ,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(aw,CO2_dissolved_,myZ,col = 'green',xlab='aw',ylab='CO2_dissolved_',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
