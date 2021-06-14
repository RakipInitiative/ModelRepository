############################# 
# start of Visualisation script Gropin ID 262 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,Ac)
argPar3 <- expand.grid(pH,Ac)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],Ac[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,Ac,z2,col = 'green',xlab='T',ylab='Ac',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,Ac,z3,col = 'green',xlab='pH',ylab='Ac',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Lactic acid bacteria in/on Deli salads _Fava-, Pepper-, Cheese salad_
(gropin ID:262)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
