#############################
# start of Parameter script
#############################
T <- seq(22.022,41.958041958042,length.out=10)
pH <- seq(5.005,7.99200799200799,length.out=10)
Oleo <- seq(0,0.799200799200799,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 351 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Oleo)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,Oleo) {
   mumax <-(-5.348+0.162*T+0.319*pH+0.066*Oleo-0.0061*T*pH-0.00091*(T^2)-0.561*(Oleo^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Oleo']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 351 
#############################
titleText <-'Response surface ln_mu_max for
Salmonella spp. in/on Brain Heart Infusion broth
(gropin ID:351)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,Oleo))
argPar3 <- unique.data.frame(expand.grid(pH,Oleo))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],Oleo = Oleo[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],Oleo = argPar2[2],pH = pH[1])),nrow=10)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],Oleo = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(Oleo)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,Oleo,z2,col = 'green',xlab='T',ylab='Oleo',zlab='ln_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,Oleo,z3,col = 'green',xlab='pH',ylab='Oleo',zlab='ln_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,Oleo))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(Oleo)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: Oleo =',round(Oleo,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,Oleo,myZ,col = 'green',xlab='T',ylab='Oleo',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,Oleo,myZ,col = 'green',xlab='pH',ylab='Oleo',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
