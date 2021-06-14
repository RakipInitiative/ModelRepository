#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
aw <- seq(0.974974,0.991008991008991,length.out=21)
CO2_dissolved_ <- seq(0,2408.59140859141,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 26 
#############################
 
# constant coefficients for this model
Im <- 74.6
m1 <- -0.74
m2 <- 5.7000000000000002E-3
m3 <- -151
m4 <- 0.000124
m5 <- 4.62e-08
m6 <- 77
m7 <- -1e-05
m8 <- 0.76
m9 <- -0.0057
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(Im+(m1*T)+(m2*CO2_dissolved_)+(m3*aw)+(m4*(T^2))+(m5*(CO2_dissolved_^2))+(m6*(aw^2))+(m7*T*CO2_dissolved_)+(m8*T*aw)+(m9*CO2_dissolved_*aw))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 26 
#############################
argPar1 <- expand.grid(T,aw)
argPar2 <- expand.grid(T,CO2_dissolved_)
argPar3 <- expand.grid(aw,CO2_dissolved_)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],CO2_dissolved_[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],aw[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,aw,z1,col = 'green',xlab='T',ylab='aw',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,CO2_dissolved_,z2,col = 'green',xlab='T',ylab='CO2_dissolved_',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(aw,CO2_dissolved_,z3,col = 'green',xlab='aw',ylab='CO2_dissolved_',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Aeromonas hydrophila in/on modified BHI
(gropin ID:26)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
