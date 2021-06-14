#############################
# start of Parameter script
#############################
T <- seq(4.004,11.988011988012,length.out=21)
aw <- seq(0.961038961038961,0.986986,length.out=21)
CO2_dissolved_ <- seq(0,2408.59140859141,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 154 
#############################
 
# constant coefficients for this model
l <- 6.04e08
l1 <- -504
l2 <- 0.16
l3 <- -5700
l4 <- 0.83
l5 <- 3.28e-06
l6 <- -6.04e08
l7 <- -0.00107
l8 <- 495
l9 <- -0.16
 
variables <- data.frame(T,aw,CO2_dissolved_)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,aw,CO2_dissolved_) {
   mumax <-(l+(l1*T)+(l2*CO2_dissolved_)+(l3*aw)+(l4*(T^2))+(l5*(CO2_dissolved_^2))+(l6*(aw^2))+(l7*T*CO2_dissolved_)+(l8*T*aw)+(l9*CO2_dissolved_*aw))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['aw'],argumentsPar['CO2_dissolved_']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 154 
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
Lactobacillus sake in/on Cooked meat model _in modified BHI_
(gropin ID:154)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
