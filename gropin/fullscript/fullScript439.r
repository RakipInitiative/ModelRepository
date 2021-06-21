#############################
# start of Parameter script
#############################
CO2 <- seq(10.01,69.9300699300699,length.out=10)
NaCl <- seq(0,2.4975024975025,length.out=10)
NaNO2 <- seq(0,74.9250749250749,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 439 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(CO2,NaCl,NaNO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(CO2,NaCl,NaNO2) {
   mumax <-1-0.4*NaNO2+0.3*CO2*NaNO2-0.5*(NaCl^2)-0.4*(CO2^2)

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['CO2'],argumentsPar['NaCl'],argumentsPar['NaNO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 439 
#############################
titleText <-'Response surface _mu_max for
Clostridium botulinum in/on Tryptone-Peptone-Yeast-C
(gropin ID:439)'
argPar1 <- unique.data.frame(expand.grid(CO2,NaCl))
argPar2 <- unique.data.frame(expand.grid(CO2,NaNO2))
argPar3 <- unique.data.frame(expand.grid(NaCl,NaNO2))
z1 <- matrix(unlist(response_surface(CO2 = argPar1[1],NaCl = argPar1[2],NaNO2 = NaNO2[1])),nrow=10)
z2 <- matrix(unlist(response_surface(CO2 = argPar2[1],NaNO2 = argPar2[2],NaCl = NaCl[1])),nrow=10)
z3 <- matrix(unlist(response_surface(NaCl = argPar3[1],NaNO2 = argPar3[2],CO2 = CO2[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(CO2)>1 & length(NaCl)>1 & length(NaNO2)>1) {
	par(mfrow = c(1,3))
	persp(CO2,NaCl,z1,col = 'green',xlab='CO2',ylab='NaCl',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(CO2,NaNO2,z2,col = 'green',xlab='CO2',ylab='NaNO2',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(NaCl,NaNO2,z3,col = 'green',xlab='NaCl',ylab='NaNO2',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(CO2,NaCl,NaNO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(NaNO2)==1) {
		persp(CO2,NaCl,myZ,col = 'green',xlab='CO2',ylab='NaCl',zlab='_mu_max',main=titleText,sub=paste('other variable: NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(NaCl)==1) {
		persp(CO2,NaNO2,myZ,col = 'green',xlab='CO2',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: NaCl =',round(NaCl,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(CO2)==1) {
		persp(NaCl,NaNO2,myZ,col = 'green',xlab='NaCl',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
