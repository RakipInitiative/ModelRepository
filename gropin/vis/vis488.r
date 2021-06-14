############################# 
# start of Visualisation script Gropin ID 488 
#############################
argPar1 <- expand.grid(S_S,Ac)
argPar2 <- expand.grid(S_S,pH)
argPar3 <- expand.grid(Ac,pH)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],pH[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],Ac[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(S_S[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(S_S,Ac,z1,col = 'green',xlab='S_S',ylab='Ac',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(S_S,pH,z2,col = 'green',xlab='S_S',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(Ac,pH,z3,col = 'green',xlab='Ac',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Zygosaccharomyces bailii in/on Yeast Nitrogen Broth
(gropin ID:488)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
