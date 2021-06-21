#############################
# start of Parameter script
#############################
pH <- seq(4.5045,5.4945054945055,length.out=10)
aw <- seq(0.97097,0.991008991008991,length.out=10)
Gelatin <- seq(0,4.995004995005,length.out=10)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 357 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw,Gelatin)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(pH,aw,Gelatin) {
   mumax <-4.455*sqrt(aw-0.9488)*sqrt(1-10^(4.135-pH))*sqrt(0.6313+(1-0.6313)*(0.4027/(0.4027+Gelatin)))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Gelatin']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 357 
#############################
titleText <-'Response surface Sqr_mu_max for
Salmonella spp. in/on Tryptic Soy Broth
(gropin ID:357)'
argPar1 <- unique.data.frame(expand.grid(pH,aw))
argPar2 <- unique.data.frame(expand.grid(pH,Gelatin))
argPar3 <- unique.data.frame(expand.grid(aw,Gelatin))
z1 <- matrix(unlist(response_surface(pH = argPar1[1],aw = argPar1[2],Gelatin = Gelatin[1])),nrow=10)
z2 <- matrix(unlist(response_surface(pH = argPar2[1],Gelatin = argPar2[2],aw = aw[1])),nrow=10)
z3 <- matrix(unlist(response_surface(aw = argPar3[1],Gelatin = argPar3[2],pH = pH[1])),nrow=10)
# adding precaution if response surface is zero
myZLim1 <- range(z1)
if(myZLim1[1] == myZLim1[2]) myZLim1[2] <- myZLim1[2]+1
myZLim2 <- range(z2)
if(myZLim2[1] == myZLim2[2]) myZLim2[2] <- myZLim2[2]+1
myZLim3 <- range(z3)
if(myZLim3[1] == myZLim3[2]) myZLim3[2] <- myZLim3[2]+1
if(length(pH)>1 & length(aw)>1 & length(Gelatin)>1) {
	par(mfrow = c(1,3))
	persp(pH,aw,z1,col = 'green',xlab='pH',ylab='aw',zlab='Sqr_mu_max',zlim=myZLim1,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,Gelatin,z2,col = 'green',xlab='pH',ylab='Gelatin',zlab='Sqr_mu_max',zlim=myZLim2,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(aw,Gelatin,z3,col = 'green',xlab='aw',ylab='Gelatin',zlab='Sqr_mu_max',zlim=myZLim3,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.2, line=-8.5, side=3)
} else {
		myPars <- unique.data.frame(expand.grid(pH,aw,Gelatin))
		myZ <-matrix(unlist(response_surface(myPars[,1],myPars[,2],myPars[,3])),nrow=10)
		myZLim <- range(myZ)
if(myZLim[1] == myZLim[2]) myZLim[2] <- myZLim[2]+1
	if(length(Gelatin)==1) {
		persp(pH,aw,myZ,col = 'green',xlab='pH',ylab='aw',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: Gelatin =',round(Gelatin,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(aw)==1) {
		persp(pH,Gelatin,myZ,col = 'green',xlab='pH',ylab='Gelatin',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: aw =',round(aw,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(aw,Gelatin,myZ,col = 'green',xlab='aw',ylab='Gelatin',zlab='Sqr_mu_max',main=titleText,sub=paste('other variable: pH =',round(pH,digits = 2)),zlim=myZLim,theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
