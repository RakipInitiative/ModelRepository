############################# 
# start of Visualisation script Gropin ID 1033 
#############################
argPar1 <- expand.grid(Fructose,Ethanol)
argPar2 <- expand.grid(Fructose,pH)
argPar3 <- expand.grid(Ethanol,pH)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],pH[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],Ethanol[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(Fructose[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(Fructose,Ethanol,z1,col = 'green',xlab='Fructose',ylab='Ethanol',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(Fructose,pH,z2,col = 'green',xlab='Fructose',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(Ethanol,pH,z3,col = 'green',xlab='Ethanol',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Saccharomyces cerevisiae in/on Wines, beverages, fruit concentrates, preserves
(gropin ID:1033)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
