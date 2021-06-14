############################# 
# start of Visualisation script Gropin ID 351 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,Oleo)
argPar3 <- expand.grid(pH,Oleo)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],Oleo[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,Oleo,z2,col = 'green',xlab='T',ylab='Oleo',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,Oleo,z3,col = 'green',xlab='pH',ylab='Oleo',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Salmonella spp. in/on Brain Heart Infusion broth
(gropin ID:351)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
