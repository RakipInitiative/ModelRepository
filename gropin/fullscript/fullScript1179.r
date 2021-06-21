#############################
# start of Parameter script
#############################
T <- seq(7.007,18.981018981019,length.out=7)
pH <- seq(4.5045,8.49150849150849,length.out=7)
NaCl <- seq(0,7.99200799200799,length.out=7)
NaNO2 <- seq(0,199.8001998002,length.out=7)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1179 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,NaCl,NaNO2) {
   mumax <-(-1.107-0.00268*T+0.299*pH+0.01432*NaCl+0.00216*T*T-0.02421*pH*pH-0.00421*NaCl*NaCl-0.00419*T*NaCl+0.00882*pH*NaCl)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1179 
#############################
titleText <-'Response surface Sqr_mu_max for
Staphylococcus aureus in/on TSB
(gropin ID:1179)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,NaCl))
argPar3 <- unique.data.frame(expand.grid(T,NaNO2))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],NaCl = NaCl[1],NaNO2 = NaNO2[1])),nrow=7)
z2 <- matrix(unlist(response_surface(T = argPar2[1],NaCl = argPar2[2],pH = pH[1],NaNO2 = NaNO2[1])),nrow=7)
z3 <- matrix(unlist(response_surface(T = argPar3[1],NaNO2 = argPar3[2],pH = pH[1],NaCl = NaCl[1])),nrow=7)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(NaCl)>1 & length(NaNO2)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='Sqr_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaCl,z2,col = 'green',xlab='T',ylab='NaCl',zlab='Sqr_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,NaNO2,z3,col = 'green',xlab='T',ylab='NaNO2',zlab='Sqr_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,NaCl,NaNO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4])),nrow=7)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(NaCl)==1 & length(NaNO2)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(NaNO2)==1) {
		persp(T,NaCl,myZ,col = 'green',xlab='T',ylab='NaCl',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(NaCl)==1) {
		persp(T,NaNO2,myZ,col = 'green',xlab='T',ylab='NaNO2',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaNO2)==1) {
		persp(pH,NaCl,myZ,col = 'green',xlab='pH',ylab='NaCl',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(NaCl)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1) {
		persp(NaCl,NaNO2,myZ,col = 'green',xlab='NaCl',ylab='NaNO2',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
