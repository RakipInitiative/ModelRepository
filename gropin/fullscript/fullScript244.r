#############################
# start of Parameter script
#############################
T <- seq(20.02,39.96003996004,length.out=21)
pH <- seq(3.5035,4.995004995005,length.out=21)
aw <- seq(0.919919,0.969030969030969,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 244 
#############################
 
# constant coefficients for this model
a0 <- -10.0315
ax <- 0.324918
ax2 <- -6.7600000000000004E-3
ay <- 1.048619
ay2 <- -0.12821
abw <- 56.47651
abw2 <- -156.337
axy <- 0.015621
axbw <- 0.358186
aybw <- -2.93541
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-a0+(ax*T)+(ax2*(T^2))+(ay*pH)+(ay2*(pH^2))+(abw*sqrt(1-aw))+(abw2*(sqrt(1-aw)^2))+(axy*T*pH)+(axbw*T*sqrt(1-aw))+(aybw*pH*sqrt(1-aw))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 244 
#############################
argPar1 <- expand.grid(T,pH)
argPar2 <- expand.grid(T,aw)
argPar3 <- expand.grid(pH,aw)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],aw[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],pH[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,pH,z1,col = 'green',xlab='T',ylab='pH',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,aw,z2,col = 'green',xlab='T',ylab='aw',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(pH,aw,z3,col = 'green',xlab='pH',ylab='aw',zlab='ln_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface ln_mu_max for
Monascus ruber in/on Malt extract agar
(gropin ID:244)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
