#############################
# start of Parameter script
#############################
S_S <- seq(0.83083,2.4975024975025,length.out=21)
Ac <- seq(1.8018,2.7972027972028,length.out=21)
pH <- seq(3.5035,3.996003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 488 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(S_S,Ac,pH)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(S_S,Ac,pH) {
   mumax <-(4.53+3.29*((S_S-1.66)/0.6)+2.94*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)+0.36*(((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4)^2)+0.82*((S_S-1.66)/0.6)*((Ac*((10^-pH)/((10^-pH)+(10^-4.75)))-2.08)/0.4))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['S_S'],argumentsPar['Ac'],argumentsPar['pH']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
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
