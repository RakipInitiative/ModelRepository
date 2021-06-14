############################# 
# start of Visualisation script Gropin ID 28 
#############################
argPar1 <- expand.grid(T,aw)
argPar2 <- expand.grid(T,CO2_dissolved_)
argPar3 <- expand.grid(aw,CO2_dissolved_)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],CO2_dissolved_[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],aw[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,aw,z1,col = 'green',xlab='T',ylab='aw',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,CO2_dissolved_,z2,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(aw,CO2_dissolved_,z3,col = 'green',xlab='aw',ylab='CO2_dissolved_',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface Sqr_mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:28)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
