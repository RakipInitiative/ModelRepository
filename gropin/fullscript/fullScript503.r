#############################
# start of Parameter script
#############################
T <- seq(7.007,39.96003996004,length.out=21)
pH <- seq(4.004,6.99300699300699,length.out=21)
aw <- seq(0.9009,0.998001998001998,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 503 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,pH,aw)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,pH,aw) {
   mumax <-(-11.909+0.3649*T+(1.7832*pH)+7.00019*sqrt(1-aw)-0.00442*(T*pH)-0.00458*(T^2)-0.12539*(pH^2)-62.114*(sqrt(1-aw))^2)

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['pH'],argumentsPar['aw']))
colnames(responseSurface) <- c(colnames(argumentsPar),'lnmumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 503 
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
Salmonella spp. in/on Meat _ComBase data_
(gropin ID:503)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
