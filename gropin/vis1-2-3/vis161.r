############################# 
# start of Visualisation script Gropin ID 161 
#############################
argPar1 <- expand.grid(T,CitrA)
argPar2 <- expand.grid(T,AscorbA)
argPar3 <- expand.grid(CitrA,AscorbA)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],AscorbA[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],CitrA[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,CitrA,z1,col = 'green',xlab='T',ylab='CitrA',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,AscorbA,z2,col = 'green',xlab='T',ylab='AscorbA',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(CitrA,AscorbA,z3,col = 'green',xlab='CitrA',ylab='AscorbA',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Listeria monocytogenes in/on Tryptic Soy Broth
(gropin ID:161)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
