############################# 
# start of Visualisation script Gropin ID 1270 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,Hours)
argPar3 <- expand.grid(pH,Hours)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],Hours[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,Hours,z2,col = 'green',xlab='T',ylab='Hours',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,Hours,z3,col = 'green',xlab='pH',ylab='Hours',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Yersinia enterocolitica in/on Sausages  Italian-fresh pork _80%_
(gropin ID:1270)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
