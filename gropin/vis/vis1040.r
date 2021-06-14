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
