############################# 
# start of Visualisation script Gropin ID 1317 
#############################
argPar1 <- expand.grid(T,NaCl)
argPar2 <- expand.grid(T,pH)
argPar3 <- expand.grid(NaCl,pH)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],pH[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],NaCl[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,NaCl,z1,col = 'green',xlab='T',ylab='NaCl',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,pH,z2,col = 'green',xlab='T',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(NaCl,pH,z3,col = 'green',xlab='NaCl',ylab='pH',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Clostridium perfringens in/on Meat _bulk_
(gropin ID:1317)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
