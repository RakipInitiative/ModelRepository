#############################
# start of Parameter script
#############################
pH <- seq(5.5055,6.49350649350649,length.out=10)
T <- seq(4.004,9.99000999000999,length.out=10)
CO2 <- seq(0.1001,0.899100899100899,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 187 
#############################
 
# constant coefficients for this model
a1 <- 4.8719
a2 <- 0.128
a3 <- -1.19333
a4 <- 10.1099
a5 <- -0.04453
a6 <- -1.41277
a7 <- -0.24792
a8 <- 0.08419
a9 <- 1.83963
 
variables <- data.frame(pH,T,CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,T,CO2) {
   mumax <-a1+(a2*pH)+(a3*T)+(a4*CO2)+(a5*pH*T)+(a6*pH*CO2)+(a7*T*CO2)+(a8*(T^2))+(a9*(CO2^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['T'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 187 
#############################
titleText <-'Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:187)'
argPar1 <- unique.data.frame(expand.grid(pH,T))
argPar2 <- unique.data.frame(expand.grid(pH,CO2))
argPar3 <- unique.data.frame(expand.grid(T,CO2))
z1 <- matrix(unlist(response_surface(pH = argPar1[1],T = argPar1[2],CO2 = CO2[1])),nrow=10)
z2 <- matrix(unlist(response_surface(pH = argPar2[1],CO2 = argPar2[2],T = T[1])),nrow=10)
z3 <- matrix(unlist(response_surface(T = argPar3[1],CO2 = argPar3[2],pH = pH[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(pH)>1 & length(T)>1 & length(CO2)>1) {
	par(mfrow = c(1,3))
	persp(pH,T,z1,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,CO2,z2,col = 'green',xlab='pH',ylab='CO2',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,CO2,z3,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(pH,T,CO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(CO2)==1) {
		persp(pH,T,myZ,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',main=titleText,sub=paste('other variable: CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,CO2,myZ,col = 'green',xlab='pH',ylab='CO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,CO2,myZ,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
