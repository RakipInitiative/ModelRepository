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
# start of Model script Gropin ID 151 
#############################
 
# constant coefficients for this model
Im <- 0.9
m1 <- -0.61
m2 <- 4.8999999999999998E-4
m3 <- -0.91
m4 <- 0.05
m5 <- 0.05
m6 <- 0.05
m7 <- -2.3e-06
m8 <- 0.63
m9 <- -5e-04
 
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
# start of Visualisation script Gropin ID 151 
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
(gropin ID:151)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
