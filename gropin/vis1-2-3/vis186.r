############################# 
# start of Visualisation script Gropin ID 186 
#############################
argPar1 <- expand.grid(pH,T)
argPar2 <- expand.grid(pH,CO2)
argPar3 <- expand.grid(T,CO2)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],CO2[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],T[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(pH[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(pH,T,z1,col = 'green',xlab='pH',ylab='T',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,CO2,z2,col = 'green',xlab='pH',ylab='CO2',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,CO2,z3,col = 'green',xlab='T',ylab='CO2',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Listeria monocytogenes in/on Brain Heart Infusion broth
(gropin ID:186)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
