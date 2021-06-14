#############################
# start of Parameter script
#############################
pH <- seq(4.5045,5.4945054945055,length.out=21)
aw <- seq(0.97097,0.991008991008991,length.out=21)
Gelatin <- seq(0,4.995004995005,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 357 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(pH,aw,Gelatin)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(pH,aw,Gelatin) {
   mumax <-4.455*sqrt(aw-0.9488)*sqrt(1-10^(4.135-pH))*sqrt(0.6313+(1-0.6313)*(0.4027/(0.4027+Gelatin)))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['pH'],argumentsPar['aw'],argumentsPar['Gelatin']))
colnames(responseSurface) <- c(colnames(argumentsPar),'Sqrmumax')
#############################
# End of Model script
#############################
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
