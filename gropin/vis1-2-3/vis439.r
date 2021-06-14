############################# 
# start of Visualisation script Gropin ID 439 
#############################
argPar1 <- expand.grid(CO2,NaCl)
argPar2 <- expand.grid(CO2,NaNO2)
argPar3 <- expand.grid(NaCl,NaNO2)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],NaNO2[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],NaCl[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(CO2[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(CO2,NaCl,z1,col = 'green',xlab='CO2',ylab='NaCl',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(CO2,NaNO2,z2,col = 'green',xlab='CO2',ylab='NaNO2',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(NaCl,NaNO2,z3,col = 'green',xlab='NaCl',ylab='NaNO2',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Clostridium botulinum in/on Tryptone-Peptone-Yeast-C
(gropin ID:439)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
