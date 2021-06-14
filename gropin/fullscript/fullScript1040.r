#############################
# start of Parameter script
#############################
T <- seq(2.002,19.98001998002,length.out=10)
pH <- seq(4.004,7.49250749250749,length.out=10)
aw <- 0.99
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 1040 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- unique.data.frame(expand.grid(variables))
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-exp(-6.001+0.2342*T+14.12*(sqrt(1-aw))-0.003552*(T^2)+0.005554*(pH^2)-76.68*((sqrt(1-aw))^2))

	return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 1040 
#############################
titleText <-'Response surface ln_mu_max for
Yarrowia lipolytica in/on Dairy products, Sugar solutions, beverages
(gropin ID:1040)'
argPar1 <- unique.data.frame(expand.grid(T,pH))
argPar2 <- unique.data.frame(expand.grid(T,aw))
argPar3 <- unique.data.frame(expand.grid(pH,aw))
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],aw[1])),nrow=10)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[1],argPar2[2])),nrow=10)
z3 <- matrix(unlist(response_surface(T[1],argPar3[1],argPar3[2])),nrow=10)
if(length(T)>1 & length(pH)>1 & length(aw)>1) {
	par(mfrow = c(1,3))
	persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	mtext(titleText,outer=T,  cex=1.5, line=-8.5, side=3)
} else {
	if(length(aw)==1) {
		persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',main=titleText,sub=paste('other variable: aw =',aw),theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(pH)==1) {
		persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: pH =',pH),theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
	if(length(T)==1) {
		persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='ln_mu_max',main=titleText,sub=paste('other variable: T =',T),theta=305,phi=20,shade=0.25,ticktype = 'detailed')
	}
}
#############################
# End of Visualisation script
#############################
