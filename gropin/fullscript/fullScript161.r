#############################
# start of Parameter script
#############################
T <- seq(10.01,24.975024975025,length.out=21)
CitrA <- seq(0,0.3996003996004,length.out=21)
AscorbA <- seq(0,0.3996003996004,length.out=21)
#############################
# end of Parameter script
#############################
############################# 
# start of Model script Gropin ID 161 
#############################
 
# constant coefficients for this model
 
variables <- data.frame(T,CitrA,AscorbA)
argumentsPar <- expand.grid(variables)
 
# heart of the model
response_surface <- function(T,CitrA,AscorbA) {
   mumax <-(0.1873+(0.7077*T)-(0.4681*CitrA)-(0.0706*AscorbA)+(0.03353*(T^2))+(0.5058*(CitrA^2))-(1.1107*T*CitrA)-(0.4981*T*AscorbA)+(0.3896*CitrA*AscorbA))

return(mumax=mumax)
} 

# output parameters
responseSurface <- cbind(argumentsPar,response_surface(argumentsPar['T'],argumentsPar['CitrA'],argumentsPar['AscorbA']))
colnames(responseSurface) <- c(colnames(argumentsPar),'mumax')
#############################
# End of Model script
#############################
############################# 
# start of Visualisation script Gropin ID 161 
#############################
argPar1 <- expand.grid(T,CitrA)
argPar2 <- expand.grid(T,AscorbA)
argPar3 <- expand.grid(CitrA,AscorbA)
z1 <- matrix(unlist(response_surface(argPar1[1],argPar1[2],AscorbA[10])),nrow=21)
z2 <- matrix(unlist(response_surface(argPar2[1],CitrA[10],argPar2[2])),nrow=21)
z3 <- matrix(unlist(response_surface(T[10],argPar3[1],argPar3[2])),nrow=21)
par(mfrow = c(1,3))
persp(T,CitrA,z1,col = 'green',xlab='T',ylab='CitrA',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(T,AscorbA,z2,col = 'green',xlab='T',ylab='AscorbA',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
persp(CitrA,AscorbA,z3,col = 'green',xlab='CitrA',ylab='AscorbA',zlab='_mu_max',theta=305,phi=20,shade=0.25,ticktype = 'detailed')
mtext('Response surface _mu_max for
Listeria monocytogenes in/on Tryptic Soy Broth
(gropin ID:161)',outer=T,  cex=1.5, line=-8.5, side=3)
#############################
# End of Visualisation script
#############################
