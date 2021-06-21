#############################
# start of Parameter script
#############################
T <- seq(-2.83283,44.955044955045,length.out=2)
pH <- seq(4.97497,6.99300699300699,length.out=2)
aw <- seq(0.923923,0.999000999000999,length.out=2)
Phe <- seq(0,31.967032967033,length.out=2)
NaNO2 <- seq(0,348.651348651349,length.out=2)
CO2 <- seq(0,3136.76323676324,length.out=2)
UAc <- seq(0,10.1898101898102,length.out=2)
ULa <- seq(0,3.77622377622378,length.out=2)
UDiac <- seq(0,4.78521478521479,length.out=2)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 317 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw,Phe,NaNO2,CO2,UAc,ULa,UDiac)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw,Phe,NaNO2,CO2,UAc,ULa,UDiac) {
   mumax <-0.419*(((T-(-2.83))^2)/((25-(-2.83))^2))*((aw-0.923)/(1-0.923))*(1-(10^(4.97-pH)))*(1-(ULa/3.79))*((32-Phe)/32)*(((350-NaNO2)/350)^2)*((3140-CO2)/3140)*(1-sqrt(UDiac/4.8))*(1-sqrt(UAc/10.3))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Phe'],argumentsPar['NaNO2'],argumentsPar['CO2'],argumentsPar['UAc'],argumentsPar['ULa'],argumentsPar['UDiac']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 317 
#############################
titleText <-'Response surface _mu_max for
Listeria monocytogenes in/on All foods _generic_
(gropin ID:317)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,aw))
argPar3 <- unique.data.frame(expand.grid(T,Phe))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],aw = aw[1],Phe = Phe[1],NaNO2 = NaNO2[1],CO2 = CO2[1],UAc = UAc[1],ULa = ULa[1],UDiac = UDiac[1])),nrow=2)
z2 <- matrix(unlist(response_surface(T = argPar2[1],aw = argPar2[2],pH = pH[1],Phe = Phe[1],NaNO2 = NaNO2[1],CO2 = CO2[1],UAc = UAc[1],ULa = ULa[1],UDiac = UDiac[1])),nrow=2)
z3 <- matrix(unlist(response_surface(T = argPar3[1],Phe = argPar3[2],pH = pH[1],aw = aw[1],NaNO2 = NaNO2[1],CO2 = CO2[1],UAc = UAc[1],ULa = ULa[1],UDiac = UDiac[1])),nrow=2)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(aw)>1 & length(Phe)>1 & length(NaNO2)>1 & length(CO2)>1 & length(UAc)>1 & length(ULa)>1 & length(UDiac)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,Phe,z3,col = 'green',xlab='T',ylab='Phe',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,aw,Phe,NaNO2,CO2,UAc,ULa,UDiac))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3],myPars[,4],myPars[,5],myPars[,6],myPars[,7],myPars[,8],myPars[,9])),nrow=2)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(T,aw,myZ,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(T,Phe,myZ,col = 'green',xlab='T',ylab='Phe',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(T,NaNO2,myZ,col = 'green',xlab='T',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(T,CO2,myZ,col = 'green',xlab='T',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(T,UAc,myZ,col = 'green',xlab='T',ylab='UAc',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(UDiac)==1) {
		persp(T,ULa,myZ,col = 'green',xlab='T',ylab='ULa',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1) {
		persp(T,UDiac,myZ,col = 'green',xlab='T',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(pH,aw,myZ,col = 'green',xlab='pH',ylab='aw',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(pH,Phe,myZ,col = 'green',xlab='pH',ylab='Phe',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(Phe)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(pH,NaNO2,myZ,col = 'green',xlab='pH',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(pH,CO2,myZ,col = 'green',xlab='pH',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(pH,UAc,myZ,col = 'green',xlab='pH',ylab='UAc',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(UDiac)==1) {
		persp(pH,ULa,myZ,col = 'green',xlab='pH',ylab='ULa',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1) {
		persp(pH,UDiac,myZ,col = 'green',xlab='pH',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(aw,Phe,myZ,col = 'green',xlab='aw',ylab='Phe',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(Phe)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(aw,NaNO2,myZ,col = 'green',xlab='aw',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(Phe)==1 & length(NaNO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(aw,CO2,myZ,col = 'green',xlab='aw',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(aw,UAc,myZ,col = 'green',xlab='aw',ylab='UAc',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(UDiac)==1) {
		persp(aw,ULa,myZ,col = 'green',xlab='aw',ylab='ULa',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1) {
		persp(aw,UDiac,myZ,col = 'green',xlab='aw',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(Phe,NaNO2,myZ,col = 'green',xlab='Phe',ylab='NaNO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(NaNO2)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(Phe,CO2,myZ,col = 'green',xlab='Phe',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(NaNO2)==1 & length(CO2)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(Phe,UAc,myZ,col = 'green',xlab='Phe',ylab='UAc',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(UDiac)==1) {
		persp(Phe,ULa,myZ,col = 'green',xlab='Phe',ylab='ULa',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1) {
		persp(Phe,UDiac,myZ,col = 'green',xlab='Phe',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(UAc)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(NaNO2,CO2,myZ,col = 'green',xlab='NaNO2',ylab='CO2',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(CO2)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(NaNO2,UAc,myZ,col = 'green',xlab='NaNO2',ylab='UAc',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(CO2)==1 & length(UAc)==1 & length(UDiac)==1) {
		persp(NaNO2,ULa,myZ,col = 'green',xlab='NaNO2',ylab='ULa',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(CO2)==1 & length(UAc)==1 & length(ULa)==1) {
		persp(NaNO2,UDiac,myZ,col = 'green',xlab='NaNO2',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(ULa)==1 & length(UDiac)==1) {
		persp(CO2,UAc,myZ,col = 'green',xlab='CO2',ylab='UAc',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'ULa =',round(ULa,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(UAc)==1 & length(UDiac)==1) {
		persp(CO2,ULa,myZ,col = 'green',xlab='CO2',ylab='ULa',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(UAc)==1 & length(ULa)==1) {
		persp(CO2,UDiac,myZ,col = 'green',xlab='CO2',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'UAc =',round(UAc,digits = 2), 'ULa =',round(ULa,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UDiac)==1) {
		persp(UAc,ULa,myZ,col = 'green',xlab='UAc',ylab='ULa',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UDiac =',round(UDiac,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(ULa)==1) {
		persp(UAc,UDiac,myZ,col = 'green',xlab='UAc',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'ULa =',round(ULa,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1 & length(pH)==1 & length(aw)==1 & length(Phe)==1 & length(NaNO2)==1 & length(CO2)==1 & length(UAc)==1) {
		persp(ULa,UDiac,myZ,col = 'green',xlab='ULa',ylab='UDiac',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2), 'pH =',round(pH,digits = 2), 'aw =',round(aw,digits = 2), 'Phe =',round(Phe,digits = 2), 'NaNO2 =',round(NaNO2,digits = 2), 'CO2 =',round(CO2,digits = 2), 'UAc =',round(UAc,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
