#############################
# start of Parameter script
#############################
T <- seq(8.008,19.98001998002,length.out=10)
pH <- seq(2.002,11.988011988012,length.out=10)
Hours <- seq(0,479.52047952048,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1273 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,Hours)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,Hours) {
   mumax <-0.898*(T-36.47)*((T-(-5.42))^2)/((31.61-(-5.42))*((31.61-(-5.42)*(T-31.61)-(31.61-36.47)*(31.61+(-5.42)-2*T)))*(pH-10.24)*((pH-5.12))/(1*((7.27-5.12*(pH-7.27)-(7.27-10.24)*(7.27+5.12-pH)))*((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.997)*(((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.924))/(1*((0.997-0.924*((1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)-0.997)-(0.997-0.997)*(0.997+0.924-(1-((17.4+0.0311*Hours+0.001036*T*Hours)/100)^2)))))))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['Hours']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1273 
#############################
titleText <-'Response surface _mu_max for
Lactic acid bacteria in/on Sausages  Italian-fresh pork _80%_
(gropin ID:1273)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,Hours))
argPar3 <- unique.data.frame(expand.grid(pH,Hours))
z1 <- matrix(unlist(response_surface(T = argPar1[1],pH = argPar1[2],Hours = Hours[1])),nrow=10)
z2 <- matrix(unlist(response_surface(T = argPar2[1],Hours = argPar2[2],pH = pH[1])),nrow=10)
z3 <- matrix(unlist(response_surface(pH = argPar3[1],Hours = argPar3[2],T = T[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(T)>1 & length(pH)>1 & length(Hours)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,Hours,z2,col = 'green',xlab='T',ylab='Hours',zlab='_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,Hours,z3,col = 'green',xlab='pH',ylab='Hours',zlab='_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(T,pH,Hours))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(Hours)==1) {
		persp(T,pH,myZ,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',main=titleText,sub=paste('other variable: Hours =',round(Hours,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,Hours,myZ,col = 'green',xlab='T',ylab='Hours',zlab='_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,Hours,myZ,col = 'green',xlab='pH',ylab='Hours',zlab='_mu_max',main=titleText,sub=paste('other variable: T =',round(T,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
