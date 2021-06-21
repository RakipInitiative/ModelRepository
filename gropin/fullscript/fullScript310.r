#############################
# start of Parameter script
#############################
T <- seq(-1.71171,45.4545454545455,length.out=4)
pH <- seq(4.72472,9.6003996003996,length.out=4)
aw <- seq(0.914914,0.999000999000999,length.out=4)
Phe <- seq(0,31.7682317682318,length.out=4)
NaNO2 <- seq(0,24.8751248751249,length.out=4)
CO2 <- seq(0,3.02697302697303,length.out=4)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 310 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,Phe,NaNO2,CO2)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,Phe,NaNO2,CO2) {
   mumax <-1.168*(((T-45.5)*((T+1.72)^2))/((37+1.72)*((37+1.72)*(T-37)-(37-45.5)*(37-1.72-2*T))))*(((pH-9.61)*(pH-4.71))/(((7.1-4.71)*(pH-7.1))-((7.1-9.61)*(4.71-pH))))*((aw-0.913)/(0.997-0.913))*(1-(NaNO2/25))*(1-(Phe/31.9))*(1-(CO2/3.04))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Phe'],argumentsPar['NaNO2'],argumentsPar['CO2']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 310 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on Poultry
(gropin ID:310)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,aw))
argPar3 <- unique.data.frame(expand.grid(T,Phe))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],aw = aw[1],Phe = Phe[1],NaNO2 = NaNO2[1],CO2 = CO2[1])),nrow=4)
z2 <- matrix(unlist(response_surface(T = argPar2[1],aw = argPar2[2],pH = pH[1],Phe = Phe[1],NaNO2 = NaNO2[1],CO2 = CO2[1])),nrow=4)
z3 <- matrix(unlist(response_surface(T = argPar3[1],Phe = argPar3[2],pH = pH[1],aw = aw[1],NaNO2 = NaNO2[1],CO2 = CO2[1])),nrow=4)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(aw)>1 & length(Phe)>1 & length(NaNO2)>1 & length(CO2)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,Phe,z3,col = 'green',xlab='T',ylab='Phe',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,aw,Phe,NaNO2,CO2))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4],myPars[,5],myPars[,6])),nrow=4)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(NaNO2)==1 & length(CO2)==1) {
		persp(T,Phe,myZ,col = 'green',xlab='T',ylab='Phe',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(CO2)==1) {
		persp(T,NaNO2,myZ,col = 'green',xlab='T',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1) {
		persp(T,CO2,myZ,col = 'green',xlab='T',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1) {
		persp(pH,aw,myZ,col = 'green',xlab='pH',ylab='aw',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(NaNO2)==1 & length(CO2)==1) {
		persp(pH,Phe,myZ,col = 'green',xlab='pH',ylab='Phe',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(Phe)==1 & length(CO2)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1) {
		persp(pH,CO2,myZ,col = 'green',xlab='pH',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(NaNO2)==1 & length(CO2)==1) {
		persp(aw,Phe,myZ,col = 'green',xlab='aw',ylab='Phe',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(Phe)==1 & length(CO2)==1) {
		persp(aw,NaNO2,myZ,col = 'green',xlab='aw',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(Phe)==1 & length(NaNO2)==1) {
		persp(aw,CO2,myZ,col = 'green',xlab='aw',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(CO2)==1) {
		persp(Phe,NaNO2,myZ,col = 'green',xlab='Phe',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'CO2 =',round(CO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(NaNO2)==1) {
		persp(Phe,CO2,myZ,col = 'green',xlab='Phe',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1) {
		persp(NaNO2,CO2,myZ,col = 'green',xlab='NaNO2',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
