############################# 
# start of Visualisation script Gropin ID 357 
#############################
argPar1 <- expand.grid(pH,aw)
argPar2 <- expand.grid(pH,Gelatin)
argPar3 <- expand.grid(aw,Gelatin)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],Gelatin[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],aw[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(pH[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(pH,aw,z1,col = 'green',xlab='pH',ylab='aw',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,Gelatin,z2,col = 'green',xlab='pH',ylab='Gelatin',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(aw,Gelatin,z3,col = 'green',xlab='aw',ylab='Gelatin',zlab='Sqr_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface Sqr_mu_max for
Salmonella spp. in/on Tryptic Soy Broth
(gropin ID:357)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
